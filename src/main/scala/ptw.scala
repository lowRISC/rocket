// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._
import cde.{Parameters, Field}

class PTWReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width = vpnBits)
  val prv = Bits(width = 2)
  val store = Bool()
  val fetch = Bool()
}

class PTWResp(implicit p: Parameters) extends CoreBundle()(p) {
  val pte = new PTE
}

class TLBPTWIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Decoupled(new PTWReq)
  val resp = Valid(new PTWResp).flip
  val status = new MStatus().asInput
  val invalidate = Bool(INPUT)
}

class DatapathPTWIO(implicit p: Parameters) extends CoreBundle()(p) {
  val ptbr = UInt(INPUT, ppnBits)
  val invalidate = Bool(INPUT)
  val status = new MStatus().asInput
}

class PTE(implicit p: Parameters) extends CoreBundle()(p) {
  val ppn = Bits(width = ppnBits)
  val reserved_for_software = Bits(width = 3)
  val d = Bool()
  val r = Bool()
  val typ = Bits(width = 4)
  val v = Bool()

  def table(dummy: Int = 0) = v && typ < 2
  def leaf(dummy: Int = 0) = v && typ >= 2
  def ur(dummy: Int = 0) = leaf() && typ < 8
  def uw(dummy: Int = 0) = ur() && typ(0)
  def ux(dummy: Int = 0) = ur() && typ(1)
  def sr(dummy: Int = 0) = leaf()
  def sw(dummy: Int = 0) = leaf() && typ(0)
  def sx(dummy: Int = 0) = v && typ >= 4 && typ(1)
  def access_ok(prv: Bits, store: Bool, fetch: Bool) =
    Mux(prv(0), Mux(fetch, sx(), Mux(store, sw(), sr())), Mux(fetch, ux(), Mux(store, uw(), ur())))
}

class PTW(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestor = Vec(n, new TLBPTWIO).flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }
  
  val s_ready :: s_req :: s_wait :: s_set_dirty :: s_wait_dirty :: s_done :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(pgLevels)))

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(new PTE)
  
  val vpn_idx = Vec((0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0)))(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val pte = new PTE().fromBits(io.mem.resp.bits.data)
  val pte_addr = Cat(r_pte.ppn, vpn_idx).toUInt << log2Up(xLen/8)

  when (arb.io.out.fire()) {
    r_req := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte.ppn := io.dpath.ptbr
  }

  val (pte_cache_hit, pte_cache_data) = {
    val size = log2Up(pgLevels * 2)
    val plru = new PseudoLRU(size)
    val valid = Reg(Vec(size, Bool()))
    val validBits = valid.toBits
    val tags = Mem(size, UInt(width = paddrBits))
    val data = Mem(size, UInt(width = ppnBits))

    val hits = Vec(tags.map(_ === pte_addr)).toBits & validBits
    val hit = hits.orR
    when (io.mem.resp.valid && pte.table() && !hit) {
      val r = Mux(validBits.andR, plru.replace, PriorityEncoder(~validBits))
      valid(r) := true
      tags(r) := pte_addr
      data(r) := pte.ppn
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (reset || io.dpath.invalidate) { valid.foreach(_ := false) }

    (hit, Mux1H(hits, data))
  }

  val perm_ok = pte.access_ok(r_req.prv, r_req.store, r_req.fetch)
  val set_dirty_bit = perm_ok && (!pte.r || (r_req.store && !pte.d))
  when (io.mem.resp.valid && state === s_wait && !set_dirty_bit) {
    r_pte := pte
  }

  val pte_wdata = Wire(init=new PTE().fromBits(0))
  pte_wdata.r := true
  pte_wdata.d := r_req.store
  
  io.mem.req.valid     := state === s_req || state === s_set_dirty
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := Mux(state === s_set_dirty, M_XA_OR, M_XRD)
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := pte_addr
  io.mem.s1_data := pte_wdata.toBits
  io.mem.s1_kill := Bool(false)
  
  val r_resp_ppn = io.mem.req.bits.addr >> pgIdxBits
  val resp_ppn = Vec((0 until pgLevels-1).map(i => Cat(r_resp_ppn >> pgLevelBits*(pgLevels-i-1), r_req.addr(pgLevelBits*(pgLevels-i-1)-1,0))) :+ r_resp_ppn)(count)
  val resp_val = state === s_done

  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_val && (r_req_dest === i)
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.pte.ppn := resp_ppn
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).status := io.dpath.status
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (arb.io.out.valid) {
        state := s_req
      }
      count := UInt(0)
    }
    is (s_req) {
      when (pte_cache_hit && count < pgLevels-1) {
        io.mem.req.valid := false
        state := s_req
        count := count + 1
        r_pte.ppn := pte_cache_data
      }.elsewhen (io.mem.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.mem.s2_nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        state := s_done
        when (pte.leaf() && set_dirty_bit) {
          state := s_set_dirty
        }
        when (pte.table() && count < pgLevels-1) {
          state := s_req
          count := count + 1
        }
      }
    }
    is (s_set_dirty) {
      when (io.mem.req.ready) {
        state := s_wait_dirty
      }
    }
    is (s_wait_dirty) {
      when (io.mem.s2_nack) {
        state := s_set_dirty
      }
      when (io.mem.resp.valid) {
        state := s_req
      }
    }
    is (s_done) {
      state := s_ready
    }
  }
}
