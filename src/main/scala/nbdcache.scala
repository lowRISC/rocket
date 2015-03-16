// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._

case object StoreDataQueueDepth extends Field[Int]
case object ReplayQueueDepth extends Field[Int]
case object NMSHRs extends Field[Int]
case object LRSCCycles extends Field[Int]
case object NDTLBEntries extends Field[Int]

abstract trait L1HellaCacheParameters extends L1CacheParameters {
  val indexmsb = untagBits-1
  val indexlsb = blockOffBits
  val offsetmsb = indexlsb-1
  val offsetlsb = wordOffBits
  val doNarrowRead = coreDataBits * nWays % rowBits == 0
  val encDataBits = code.width(coreDataBits)
  val encRowBits = encDataBits*rowWords

  // support memory tags
  val memTagBits = params(TagBits)
  val taggedDataBits = params(CoreDataBits) + params(TagBits)
  val encTaggedDataBits = code.width(taggedDataBits)
  val encTaggedRowBits = encTaggedDataBits*rowWords
}

abstract class L1HellaCacheBundle extends Bundle with L1HellaCacheParameters
abstract class L1HellaCacheModule extends Module with L1HellaCacheParameters

class StoreGen(typ: Bits, addr: Bits, dat: Bits, tagWidth: Int)
{
  val byte = typ === MT_B || typ === MT_BU
  val half = typ === MT_H || typ === MT_HU
  val word = typ === MT_W || typ === MT_WU
  val tag  = typ === MT_T
  val tag_mask = SInt(-1, tagWidth).toBits
  val byte_mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
    Mux(word, Bits( 15) << Cat(addr(2),   Bits(0,2)),
              Bits(255))))
  def mask =
    Mux(tag,  tag_mask  << 64,
              FillInterleaved(8, byte_mask))
  def data =
    Mux(tag,  dat(tagWidth-1,0) << 64,
    Mux(byte, Fill(8, dat( 7,0)),
    Mux(half, Fill(4, dat(15,0)),
                      wordData)))
  lazy val wordData =
    Mux(word, Fill(2, dat(31,0)),
                      dat)
}

class LoadGen(typ: Bits, addr: Bits, dat: Bits, zero: Bool, tagWidth: Int)
{
  val t = new StoreGen(typ, addr, dat, tagWidth)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W || typ === MT_D

  val wordShift = Mux(addr(2), dat(63,32), dat(31,0))
  val word = Cat(Mux(t.word, Fill(32, sign && wordShift(31)), dat(63,32)), wordShift)
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(48, sign && halfShift(15)), word(63,16)), halfShift)
  val byteShift = Mux(zero, UInt(0), Mux(addr(0), half(15,8), half(7,0)))
  val tag = Cat(Bits(0,64-tagWidth), dat(63+tagWidth, 64))
  val byte = Mux(t.tag,  tag, Cat(Mux(zero || t.byte, Fill(56, sign && byteShift(7)), half(63,8)), byteShift))
}

class HellaCacheReq extends CoreBundle {
  val kill = Bool()
  val typ  = Bits(width = MT_SZ)
  val phys = Bool()
  val addr = UInt(width = coreMaxAddrBits)
  val data = Bits(width = coreDataBits)
  val tag  = Bits(width = coreDCacheReqTagBits)
  val cmd  = Bits(width = M_SZ)
}

class HellaCacheResp extends CoreBundle {
  val nack = Bool() // comes 2 cycles after req.fire
  val replay = Bool()
  val typ = Bits(width = MT_SZ)
  val has_data = Bool()
  val data = Bits(width = coreDataBits)
  val data_subword = Bits(width = coreDataBits)
  val tag = Bits(width = coreDCacheReqTagBits)
  val cmd  = Bits(width = 4)
  val addr = UInt(width = coreMaxAddrBits)
  val store_data = Bits(width = coreDataBits)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
}

// interface between D$ and processor/DTLB
class HellaCacheIO extends CoreBundle {
  val req = Decoupled(new HellaCacheReq)
  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Valid(Bits(width = coreDCacheReqTagBits)).flip
  val xcpt = (new HellaCacheExceptions).asInput
  val ptw = new TLBPTWIO().flip
  val ordered = Bool(INPUT)
}

class MSHRReq extends HellaCacheReq with L1HellaCacheParameters {
  val tag_match = Bool()
  val old_meta = new L1Metadata
  val way_en = Bits(width = nWays)
}

class Replay extends HellaCacheReq with L1HellaCacheParameters {
  val sdq_id = UInt(width = log2Up(params(StoreDataQueueDepth)))
}

class DataReadReq extends L1HellaCacheBundle {
  val way_en = Bits(width = nWays)
  val addr   = Bits(width = untagBits)
}

class DataWriteReq extends DataReadReq {
  val wmask  = Bits(width = rowWords)
  val data   = Bits(width = encTaggedRowBits)
}

class L1MetaReadReq extends MetaReadReq {
  val tag = Bits(width = tagBits)
}

class L1MetaWriteReq extends 
  MetaWriteReq[L1Metadata](new L1Metadata)

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata) = {
    val meta = new L1Metadata
    meta.tag := tag
    meta.coh := coh
    meta
  }
}
class L1Metadata extends Metadata with L1HellaCacheParameters {
  val coh = co.clientMetadataOnFlush.clone
}

class InternalProbe extends Probe with HasClientTransactionId

class WritebackReq extends L1HellaCacheBundle {
  val tag = Bits(width = tagBits)
  val idx = Bits(width = idxBits)
  val way_en = Bits(width = nWays)
  val client_xact_id = Bits(width = params(TLClientXactIdBits))
  val master_xact_id = Bits(width = params(TLMasterXactIdBits))
  val r_type = UInt(width = co.releaseTypeWidth) 
}

class MSHR(id: Int) extends L1HellaCacheModule {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReq().asInput
    val req_sdq_id     = UInt(INPUT, log2Up(params(StoreDataQueueDepth)))

    val idx_match       = Bool(OUTPUT)
    val tag             = Bits(OUTPUT, tagBits)

    val mem_req  = Decoupled(new Acquire)
    val mem_resp = new DataWriteReq().asOutput
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new Replay)
    val mem_grant = Valid(new LogicalNetworkIO(new Grant)).flip
    val mem_finish = Decoupled(new LogicalNetworkIO(new Finish))
    val wb_req = Decoupled(new WritebackReq)
    val probe_rdy = Bool(OUTPUT)
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)

  val acquire_type = Reg(UInt())
  val release_type = Reg(UInt())
  val line_state = Reg(new ClientMetadata()(co))
  val refill_count = Reg(UInt(width = log2Up(refillCycles))) // TODO: zero-width wire
  val req = Reg(new MSHRReq())

  val req_cmd = io.req_bits.cmd
  val req_idx = req.addr(untagBits-1,blockOffBits)
  val idx_match = req_idx === io.req_bits.addr(untagBits-1,blockOffBits)
  val sec_rdy = idx_match && (state === s_wb_req || state === s_wb_resp || state === s_meta_clear || (state === s_refill_req || state === s_refill_resp) && !co.needsTransactionOnSecondaryMiss(req_cmd, io.mem_req.bits))

  require(isPow2(refillCycles))
  val reply = io.mem_grant.valid && io.mem_grant.bits.payload.client_xact_id === UInt(id)
  val refill_done = reply && (if(refillCycles > 1) refill_count.andR else Bool(true))
  val wb_done = reply && (state === s_wb_resp)

  val meta_on_flush = co.clientMetadataOnFlush
  val meta_on_grant = co.clientMetadataOnGrant(io.mem_grant.bits.payload, io.mem_req.bits)
  val meta_on_hit = co.clientMetadataOnHit(req_cmd, io.req_bits.old_meta.coh)

  val rpq = Module(new Queue(new Replay, params(ReplayQueueDepth)))
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && !isPrefetch(req_cmd)
  rpq.io.enq.bits := io.req_bits
  rpq.io.enq.bits.sdq_id := io.req_sdq_id
  rpq.io.deq.ready := io.replay.ready && state === s_drain_rpq || state === s_invalid

  when (state === s_drain_rpq && !rpq.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write_resp) {
    // this wait state allows us to catch RAW hazards on the tags via nack_victim
    state := s_drain_rpq
  }
  when (state === s_meta_write_req && io.meta_write.ready) {
    state := s_meta_write_resp
  }
  when (state === s_refill_resp) {
    when (refill_done) { state := s_meta_write_req }
    when (reply) {
      if(refillCycles > 1) refill_count := refill_count + UInt(1)
      line_state := meta_on_grant
    }
  }
  when (io.mem_req.fire()) { // s_refill_req
    state := s_refill_resp
  }
  when (state === s_meta_clear && io.meta_write.ready) {
    state := s_refill_req
  }
  when (state === s_wb_resp && reply) {
    state := s_meta_clear
  }
  when (io.wb_req.fire()) { // s_wb_req
    state := s_wb_resp 
  }

  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    acquire_type := co.getAcquireTypeOnSecondaryMiss(req_cmd, meta_on_flush, io.mem_req.bits)
  }
  when (io.req_pri_val && io.req_pri_rdy) {
    line_state := meta_on_flush
    refill_count := UInt(0)
    acquire_type := co.getAcquireTypeOnPrimaryMiss(req_cmd, meta_on_flush)
    release_type := co.getReleaseTypeOnVoluntaryWriteback() //TODO downgrades etc 
    req := io.req_bits

    when (io.req_bits.tag_match) {
      when (co.isHit(req_cmd, io.req_bits.old_meta.coh)) { // set dirty bit
        state := s_meta_write_req
        line_state := meta_on_hit
      }.otherwise { // upgrade permissions
        state := s_refill_req
      }
    }.otherwise { // writback if necessary and refill
      state := Mux(co.needsWriteback(io.req_bits.old_meta.coh), s_wb_req, s_meta_clear)
    }
  }

  val ackq = Module(new Queue(new LogicalNetworkIO(new Finish), 1))
  ackq.io.enq.valid := (wb_done || refill_done) && co.requiresAckForGrant(io.mem_grant.bits.payload.g_type)
  ackq.io.enq.bits.payload.master_xact_id := io.mem_grant.bits.payload.master_xact_id
  ackq.io.enq.bits.header.dst := io.mem_grant.bits.header.src
  val can_finish = state === s_invalid || state === s_refill_req || state === s_refill_resp
  io.mem_finish.valid := ackq.io.deq.valid && can_finish
  ackq.io.deq.ready := io.mem_finish.ready && can_finish
  io.mem_finish.bits := ackq.io.deq.bits

  io.idx_match := (state != s_invalid) && idx_match
  io.mem_resp := req
  io.mem_resp.addr := (if(refillCycles > 1) Cat(req_idx, refill_count) else req_idx) << rowOffBits
  io.tag := req.addr >> untagBits
  io.req_pri_rdy := state === s_invalid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  val meta_hazard = Reg(init=UInt(0,2))
  when (meta_hazard != UInt(0)) { meta_hazard := meta_hazard + 1 }
  when (io.meta_write.fire()) { meta_hazard := 1 }
  io.probe_rdy := !idx_match || (state != s_wb_req && state != s_wb_resp && state != s_meta_clear && meta_hazard === 0)

  io.meta_write.valid := state === s_meta_write_req || state === s_meta_clear
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.data.coh := Mux(state === s_meta_clear, meta_on_flush, line_state)
  io.meta_write.bits.data.tag := io.tag
  io.meta_write.bits.way_en := req.way_en

  io.wb_req.valid := state === s_wb_req && ackq.io.enq.ready
  io.wb_req.bits.tag := req.old_meta.tag
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.way_en := req.way_en
  io.wb_req.bits.client_xact_id := Bits(id)
  io.wb_req.bits.master_xact_id := Bits(0) // DNC
  io.wb_req.bits.r_type := co.getReleaseTypeOnVoluntaryWriteback()

  io.mem_req.valid := state === s_refill_req && ackq.io.enq.ready
  io.mem_req.bits.a_type := acquire_type
  io.mem_req.bits.addr := Cat(io.tag, req_idx).toUInt
  io.mem_req.bits.client_xact_id := Bits(id)
  io.mem_finish <> ackq.io.deq

  io.meta_read.valid := state === s_drain_rpq
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := io.tag

  io.replay.valid := state === s_drain_rpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := Bool(true)
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0)).toUInt

  when (!io.meta_read.ready) {
    rpq.io.deq.ready := Bool(false)
    io.replay.bits.cmd := M_NOP
  }
}

class MSHRFile extends L1HellaCacheModule {
  val io = new Bundle {
    val req = Decoupled(new MSHRReq).flip
    val secondary_miss = Bool(OUTPUT)

    val mem_req  = Decoupled(new Acquire)
    val mem_resp = new DataWriteReq().asOutput
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new Replay)
    val mem_grant = Valid(new LogicalNetworkIO(new Grant)).flip
    val mem_finish = Decoupled(new LogicalNetworkIO(new Finish))
    val wb_req = Decoupled(new WritebackReq)

    val probe_rdy = Bool(OUTPUT)
    val fence_rdy = Bool(OUTPUT)
  }

  val sdq_val = Reg(init=Bits(0, params(StoreDataQueueDepth)))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(params(StoreDataQueueDepth)-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && isWrite(io.req.bits.cmd)
  val sdq = Mem(io.req.bits.data, params(StoreDataQueueDepth))
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Vec.fill(params(NMSHRs)){Bool()}
  val tagList = Vec.fill(params(NMSHRs)){Bits()}
  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> untagBits

  val wbTagList = Vec.fill(params(NMSHRs)){Bits()}
  val memRespMux = Vec.fill(params(NMSHRs)){new DataWriteReq}
  val meta_read_arb = Module(new Arbiter(new L1MetaReadReq, params(NMSHRs)))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq, params(NMSHRs)))
  val mem_req_arb = Module(new Arbiter(new Acquire, params(NMSHRs)))
  val mem_finish_arb = Module(new Arbiter(new LogicalNetworkIO(new Finish), params(NMSHRs)))
  val wb_req_arb = Module(new Arbiter(new WritebackReq, params(NMSHRs)))
  val replay_arb = Module(new Arbiter(new Replay, params(NMSHRs)))
  val alloc_arb = Module(new Arbiter(Bool(), params(NMSHRs)))

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var sec_rdy = Bool(false)

  io.fence_rdy := true
  io.probe_rdy := true

  for (i <- 0 until params(NMSHRs)) {
    val mshr = Module(new MSHR(i))

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits := io.req.bits
    mshr.io.req_sdq_id := sdq_alloc_id

    mshr.io.meta_read <> meta_read_arb.io.in(i)
    mshr.io.meta_write <> meta_write_arb.io.in(i)
    mshr.io.mem_req <> mem_req_arb.io.in(i)
    mshr.io.mem_finish <> mem_finish_arb.io.in(i)
    mshr.io.wb_req <> wb_req_arb.io.in(i)
    mshr.io.replay <> replay_arb.io.in(i)

    mshr.io.mem_grant <> io.mem_grant
    memRespMux(i) := mshr.io.mem_resp

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) { io.fence_rdy := false }
    when (!mshr.io.probe_rdy) { io.probe_rdy := false }
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && !idx_match

  meta_read_arb.io.out <> io.meta_read
  meta_write_arb.io.out <> io.meta_write
  mem_req_arb.io.out <> io.mem_req
  mem_finish_arb.io.out <> io.mem_finish
  wb_req_arb.io.out <> io.wb_req

  io.req.ready := Mux(idx_match, tag_match && sec_rdy, pri_rdy) && sdq_rdy
  io.secondary_miss := idx_match
  io.mem_resp := memRespMux(io.mem_grant.bits.payload.client_xact_id)

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(io.replay.bits.sdq_id) & Fill(params(StoreDataQueueDepth), free_sdq)) | 
               PriorityEncoderOH(~sdq_val(params(StoreDataQueueDepth)-1,0)) & Fill(params(StoreDataQueueDepth), sdq_enq)
  }
}

class WritebackUnit extends L1HellaCacheModule {
  val io = new Bundle {
    val req = Decoupled(new WritebackReq()).flip
    val meta_read = Decoupled(new L1MetaReadReq)
    val data_req = Decoupled(new DataReadReq())
    val data_resp = Bits(INPUT, encTaggedRowBits)
    val release = Decoupled(new Release)
  }

  val active = Reg(init=Bool(false))
  val r1_data_req_fired = Reg(init=Bool(false))
  val r2_data_req_fired = Reg(init=Bool(false))
  val cnt = Reg(init = UInt(0, width = log2Up(refillCycles+1)))
  val req = Reg(new WritebackReq)

  io.release.valid := false
  when (active) {
    r1_data_req_fired := false
    r2_data_req_fired := r1_data_req_fired
    when (io.data_req.fire() && io.meta_read.fire()) {
      r1_data_req_fired := true
      cnt := cnt + 1
    }
    if(refillCycles > 1) { // Coalescing buffer inserted
      when (!r1_data_req_fired && !r2_data_req_fired && cnt === refillCycles) {
        io.release.valid := true
        active := !io.release.ready
      }
    } else {                    // No buffer, data released a cycle earlier
      when (r2_data_req_fired) {
        io.release.valid := true
        when(!io.release.ready) {
          r1_data_req_fired := false
          r2_data_req_fired := false
          cnt := UInt(0)
        } .otherwise {
          active := false
        }
      }
    }
  }
  when (io.req.fire()) {
    active := true
    cnt := 0
    req := io.req.bits
  }

  val fire = active && cnt < UInt(refillCycles)
  io.req.ready := !active

  // We reissue the meta read as it sets up the muxing for s2_data_muxed
  io.meta_read.valid := fire
  io.meta_read.bits.idx := req.idx
  io.meta_read.bits.tag := req.tag

  io.data_req.valid := fire
  io.data_req.bits.way_en := req.way_en
  if(refillCycles > 1) {
    io.data_req.bits.addr := Cat(req.idx, cnt(log2Up(refillCycles)-1,0)) << rowOffBits
  } else {
    io.data_req.bits.addr := req.idx << rowOffBits
  }

  io.release.bits.r_type := req.r_type
  io.release.bits.addr := Cat(req.tag, req.idx).toUInt
  io.release.bits.client_xact_id := req.client_xact_id
  io.release.bits.master_xact_id := req.master_xact_id
  if(refillCycles > 1) {
    val data_buf = Reg(Bits())
    when(active && r2_data_req_fired) {
      data_buf := Cat(io.data_resp, data_buf(refillCycles*encTaggedRowBits-1, encTaggedRowBits))
    }
    io.release.bits.data := data_buf
  } else {
    io.release.bits.data := io.data_resp
  }

}

class ProbeUnit extends L1HellaCacheModule {
  val io = new Bundle {
    val req = Decoupled(new InternalProbe).flip
    val rep = Decoupled(new Release)
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq)
    val way_en = Bits(INPUT, nWays)
    val mshr_rdy = Bool(INPUT)
    val line_state = new ClientMetadata()(co).asInput
  }

  val s_reset :: s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req :: s_release :: s_writeback_req :: s_writeback_resp :: s_meta_write :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)
  val line_state = Reg(co.clientMetadataOnFlush.clone)
  val way_en = Reg(Bits())
  val req = Reg(new InternalProbe)
  val hit = way_en.orR

  when (state === s_meta_write && io.meta_write.ready) {
    state := s_invalid
  }
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }
  when (state === s_writeback_req && io.wb_req.ready) {
    state := s_writeback_resp
  }
  when (state === s_release && io.rep.ready) {
    state := s_invalid
    when (hit) {
      state := Mux(co.needsWriteback(line_state), s_writeback_req, s_meta_write)
    }
  }
  when (state === s_mshr_req) {
    state := s_release
    line_state := io.line_state
    way_en := io.way_en
    when (!io.mshr_rdy) { state := s_meta_read }
  }
  when (state === s_meta_resp) {
    state := s_mshr_req
  }
  when (state === s_meta_read && io.meta_read.ready) {
    state := s_meta_resp
  }
  when (state === s_invalid && io.req.valid) {
    state := s_meta_read
    req := io.req.bits
  }
  when (state === s_reset) {
    state := s_invalid
  }

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release && !(hit && co.needsWriteback(line_state))
  io.rep.bits := Release(co.getReleaseTypeOnProbe(req, Mux(hit, line_state, co.clientMetadataOnFlush)), req.addr, req.client_xact_id, req.master_xact_id)

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.idx := req.addr
  io.meta_read.bits.tag := req.addr >> idxBits

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req.addr
  io.meta_write.bits.data.coh := co.clientMetadataOnProbe(req, line_state)
  io.meta_write.bits.data.tag := req.addr >> UInt(idxBits)

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.idx := req.addr
  io.wb_req.bits.tag := req.addr >> UInt(idxBits)
  io.wb_req.bits.r_type := co.getReleaseTypeOnProbe(req, Mux(hit, line_state, co.clientMetadataOnFlush))
  io.wb_req.bits.client_xact_id := req.client_xact_id
  io.wb_req.bits.master_xact_id := req.master_xact_id
}

class DataArray extends L1HellaCacheModule {
  val io = new Bundle {
    val read = Decoupled(new DataReadReq).flip
    val write = Decoupled(new DataWriteReq).flip
    val resp = Vec.fill(nWays){Bits(OUTPUT, encTaggedRowBits)}
  }

  val waddr = io.write.bits.addr >> rowOffBits
  val raddr = io.read.bits.addr >> rowOffBits

  if (doNarrowRead) {
    for (w <- 0 until nWays by rowWords) {
      val wway_en = io.write.bits.way_en(w+rowWords-1,w)
      val rway_en = io.read.bits.way_en(w+rowWords-1,w)
      val resp = Vec.fill(rowWords){Bits(width = encTaggedRowBits)}
      val r_raddr = RegEnable(io.read.bits.addr, io.read.valid)
      for (p <- 0 until resp.size) {
        val array = Mem(Bits(width=encTaggedRowBits), nSets*refillCycles, seqRead = true)
        when (wway_en.orR && io.write.valid && io.write.bits.wmask(p)) {
          val data = Fill(rowWords, io.write.bits.data(encTaggedDataBits*(p+1)-1,encTaggedDataBits*p))
          val mask = FillInterleaved(encTaggedDataBits, wway_en)
          array.write(waddr, data, mask)
        }
        resp(p) := array(RegEnable(raddr, rway_en.orR && io.read.valid))
      }
      for (dw <- 0 until rowWords) {
        val r = Vec(resp.map(_(encTaggedDataBits*(dw+1)-1,encTaggedDataBits*dw)))
        val resp_mux =
          if (r.size == 1) r
          else Vec(r(r_raddr(rowOffBits-1,wordOffBits)), r.tail:_*)
        io.resp(w+dw) := resp_mux.toBits
      }
    }
  } else {
    val wmask = FillInterleaved(encTaggedDataBits, io.write.bits.wmask)
    for (w <- 0 until nWays) {
      val array = Mem(Bits(width=encTaggedRowBits), nSets*refillCycles, seqRead = true)
      when (io.write.bits.way_en(w) && io.write.valid) {
        array.write(waddr, io.write.bits.data, wmask)
      }
      io.resp(w) := array(RegEnable(raddr, io.read.bits.way_en(w) && io.read.valid))
    }
  }

  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}

class AMOALU extends L1HellaCacheModule {
  val io = new Bundle {
    val addr = Bits(INPUT, blockOffBits)
    val cmd = Bits(INPUT, 4)
    val typ = Bits(INPUT, MT_SZ)
    val lhs = Bits(INPUT, taggedDataBits)
    val rhs = Bits(INPUT, taggedDataBits)
    val out = Bits(OUTPUT, taggedDataBits)
  }

  require(coreDataBits == 64)
  val storegen = new StoreGen(io.typ, io.addr, io.rhs, memTagBits)
  val rhs = storegen.wordData
  
  val sgned = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val word = io.typ === MT_W || io.typ === MT_WU || io.typ === MT_B || io.typ === MT_BU

  val mask = SInt(-1,64) ^ (io.addr(2) << 31)
  val adder_out = (io.lhs & mask).toUInt + (rhs & mask)

  val cmp_lhs  = Mux(word && !io.addr(2), io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word && !io.addr(2), rhs(31), rhs(63))
  val lt_lo = io.lhs(31,0) < rhs(31,0)
  val lt_hi = io.lhs(63,32) < rhs(63,32)
  val eq_hi = io.lhs(63,32) === rhs(63,32)
  val lt = Mux(word, Mux(io.addr(2), lt_hi, lt_lo), lt_hi || eq_hi && lt_lo)
  val less = Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))

  val out = Mux(io.cmd === M_XA_ADD, adder_out,
            Mux(io.cmd === M_XA_AND, io.lhs & rhs,
            Mux(io.cmd === M_XA_OR,  io.lhs | rhs,
            Mux(io.cmd === M_XA_XOR, io.lhs ^ rhs,
            Mux(Mux(less, min, max), io.lhs,
            storegen.data)))))

  val wmask = storegen.mask
  io.out := wmask & out | ~wmask & io.lhs
}

class HellaCache extends L1HellaCacheModule {
  val io = new Bundle {
    val cpu = (new HellaCacheIO).flip
    val mem = new TileLinkIO
  }
 
  require(params(LRSCCycles) >= 32) // ISA requires 16-insn LRSC sequences to succeed
  require(isPow2(nSets))
  require(isPow2(nWays)) // TODO: relax this
  require(params(RowBits) <= params(TLDataBits))
  require(paddrBits-blockOffBits == params(TLAddrBits) )
  require(untagBits <= pgIdxBits)

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)

  io.cpu.req.ready := Bool(true)
  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_req = Reg(io.cpu.req.bits.clone)
  val s1_valid_masked = s1_valid && !io.cpu.req.bits.kill
  val s1_replay = Reg(init=Bool(false))
  val s1_clk_en = Reg(Bool())

  val s2_valid = Reg(next=s1_valid_masked, init=Bool(false))
  val s2_req = Reg(io.cpu.req.bits.clone)
  val s2_replay = Reg(next=s1_replay, init=Bool(false)) && s2_req.cmd != M_NOP
  val s2_recycle = Bool()
  val s2_valid_masked = Bool()

  val s3_valid = Reg(init=Bool(false))
  val s3_req = Reg(io.cpu.req.bits.clone)
  val s3_req_data = Reg(Bits(width=taggedDataBits))  // do not use the data in s2_req as tag will be lost
  val s3_way = Reg(Bits())

  val s1_recycled = RegEnable(s2_recycle, s1_clk_en)
  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_sc = s1_req.cmd === M_XSC
  val s1_readwrite = s1_read || s1_write || isPrefetch(s1_req.cmd)

  val dtlb = Module(new TLB(params(NDTLBEntries)))
  dtlb.io.ptw <> io.cpu.ptw
  dtlb.io.req.valid := s1_valid_masked && s1_readwrite && !s1_req.phys
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.asid := UInt(0)
  dtlb.io.req.bits.vpn := s1_req.addr >> pgIdxBits
  dtlb.io.req.bits.instruction := Bool(false)
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := Bool(false) }
  
  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
  }
  when (wb.io.meta_read.valid) {
    s1_req.addr := Cat(wb.io.meta_read.bits.tag, wb.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (prober.io.meta_read.valid) {
    s1_req.addr := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (mshrs.io.replay.valid) {
    s1_req := mshrs.io.replay.bits
  }
  when (s2_recycle) {
    s1_req := s2_req
  }
  val s1_addr = Cat(dtlb.io.resp.ppn, s1_req.addr(pgIdxBits-1,0))

  when (s1_clk_en) {
    s2_req.kill := s1_req.kill
    s2_req.typ := s1_req.typ
    s2_req.phys := s1_req.phys
    s2_req.addr := s1_addr
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshrs.io.replay.bits.data, io.cpu.req.bits.data)
    }
    when (s1_recycled) { s2_req.data := s1_req.data }
    s2_req.tag := s1_req.tag
    s2_req.cmd := s1_req.cmd
  }

  val misaligned =
    (((s1_req.typ === MT_H) || (s1_req.typ === MT_HU)) && (s1_req.addr(0) != Bits(0))) ||
    (((s1_req.typ === MT_W) || (s1_req.typ === MT_WU)) && (s1_req.addr(1,0) != Bits(0))) ||
    (((s1_req.typ === MT_D) || (s1_req.typ === MT_T )) && (s1_req.addr(2,0) != Bits(0)))
    
  io.cpu.xcpt.ma.ld := s1_read && misaligned
  io.cpu.xcpt.ma.st := s1_write && misaligned
  io.cpu.xcpt.pf.ld := s1_read && dtlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && dtlb.io.resp.xcpt_st

  // tags
  def onReset = L1Metadata(UInt(0), ClientMetadata(UInt(0))(co))
  val meta = Module(new MetadataArray(onReset _))
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  metaReadArb.io.out <> meta.io.read
  metaWriteArb.io.out <> meta.io.write

  // data
  val data = Module(new DataArray)
  val readArb = Module(new Arbiter(new DataReadReq, 4))
  val writeArb = Module(new Arbiter(new DataWriteReq, 2))
  data.io.write.valid := writeArb.io.out.valid
  writeArb.io.out.ready := data.io.write.ready
  data.io.write.bits := writeArb.io.out.bits
  val wdata_encoded = (0 until rowWords).map(i => code.encode(writeArb.io.out.bits.data(taggedDataBits*(i+1)-1,taggedDataBits*i)))
  data.io.write.bits.data := Vec(wdata_encoded).toBits

  // tag read for new requests
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.idx := io.cpu.req.bits.addr >> blockOffBits
  when (!metaReadArb.io.in(4).ready) { io.cpu.req.ready := Bool(false) }

  // data read for new requests
  readArb.io.in(3).valid := io.cpu.req.valid
  readArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(3).bits.way_en := SInt(-1)
  when (!readArb.io.in(3).ready) { io.cpu.req.ready := Bool(false) }

  // recycled requests
  metaReadArb.io.in(0).valid := s2_recycle
  metaReadArb.io.in(0).bits.idx := s2_req.addr >> blockOffBits
  readArb.io.in(0).valid := s2_recycle
  readArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).bits.way_en := SInt(-1)

  // tag check and way muxing
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).toBits
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && co.isValid(meta.io.resp(w).coh)).toBits
  s1_clk_en := metaReadArb.io.out.valid //TODO: should be metaReadArb.io.out.fire(), but triggers Verilog backend bug
  val s1_writeback = s1_clk_en && !s1_valid && !s1_replay
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))
  val s2_hit = s2_tag_match && co.isHit(s2_req.cmd, s2_hit_state) && s2_hit_state === co.clientMetadataOnHit(s2_req.cmd, s2_hit_state)

  // load-reserved/store-conditional
  val lrsc_count = Reg(init=UInt(0))
  val lrsc_valid = lrsc_count.orR
  val lrsc_addr = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.cmd === M_XLR, s2_req.cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_valid) { lrsc_count := lrsc_count - 1 }
  when (s2_valid_masked && s2_hit || s2_replay) {
    when (s2_lr) {
      when (!lrsc_valid) { lrsc_count := params(LRSCCycles)-1 }
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (s2_sc) {
      lrsc_count := 0
    }
  }
  when (io.cpu.ptw.sret) { lrsc_count := 0 }

  val s2_data = Vec.fill(nWays){Bits(width = encTaggedRowBits)}
  for (w <- 0 until nWays) {
    val regs = Vec.fill(rowWords){Reg(Bits(width = encTaggedDataBits))}
    val en1 = s1_clk_en && s1_tag_eq_way(w)
    for (i <- 0 until regs.size) {
      val en = en1 && ((Bool(i == 0) || !Bool(doNarrowRead)) || s1_writeback)
      when (en) { regs(i) := data.io.resp(w) >> encTaggedDataBits*i }
    }
    s2_data(w) := regs.toBits
  }
  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  val s2_data_decoded = (0 until rowWords).map(i => code.decode(s2_data_muxed(encTaggedDataBits*(i+1)-1,encTaggedDataBits*i)))
  val s2_data_corrected = Vec(s2_data_decoded.map(_.corrected))
  val s2_data_uncorrected = Vec(s2_data_decoded.map(_.uncorrected))
  val s2_word_idx = if(doNarrowRead) UInt(0) else s2_req.addr(log2Up(rowWords*coreDataBytes)-1,3)
  val s2_data_correctable = Vec(s2_data_decoded.map(_.correctable)).toBits()(s2_word_idx)
  
  // store/amo hits
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && !s2_sc_fail && isWrite(s2_req.cmd)
  val amoalu = Module(new AMOALU)
  when ((s2_valid || s2_replay) && (isWrite(s2_req.cmd) || s2_data_correctable)) {
    s3_req := s2_req
    s3_req_data := Mux(s2_data_correctable, s2_data_corrected(s2_word_idx), amoalu.io.out)
    s3_way := s2_tag_match_way
  }

  writeArb.io.in(0).bits.addr := s3_req.addr
  writeArb.io.in(0).bits.wmask := UInt(1) << (if(rowOffBits > offsetlsb) 
                                                s3_req.addr(rowOffBits-1,offsetlsb).toUInt
                                              else UInt(0))
  writeArb.io.in(0).bits.data := Fill(rowWords, s3_req_data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // replacement policy
  val replacer = params(Replacer)()
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  // miss handling
  mshrs.io.req.valid := s2_valid_masked && !s2_hit && (isPrefetch(s2_req.cmd) || isRead(s2_req.cmd) || isWrite(s2_req.cmd))
  mshrs.io.req.bits := s2_req
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data := s2_req.data
  when (mshrs.io.req.fire()) { replacer.miss }

  io.mem.acquire <> DecoupledLogicalNetworkIOWrapper(mshrs.io.mem_req)

  // replays
  readArb.io.in(1).valid := mshrs.io.replay.valid
  readArb.io.in(1).bits := mshrs.io.replay.bits
  readArb.io.in(1).bits.way_en := SInt(-1)
  mshrs.io.replay.ready := readArb.io.in(1).ready
  s1_replay := mshrs.io.replay.valid && readArb.io.in(1).ready
  metaReadArb.io.in(1) <> mshrs.io.meta_read
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  // probes
  val releaseArb = Module(new Arbiter(new Release, 2))
  DecoupledLogicalNetworkIOWrapper(releaseArb.io.out) <> io.mem.release

  val probe = DecoupledLogicalNetworkIOUnwrapper(io.mem.probe)
  prober.io.req.valid := probe.valid && !lrsc_valid
  probe.ready := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits := probe.bits
  prober.io.rep <> releaseArb.io.in(1)
  prober.io.way_en := s2_tag_match_way
  prober.io.line_state := s2_hit_state
  prober.io.meta_read <> metaReadArb.io.in(2)
  prober.io.meta_write <> metaWriteArb.io.in(1)
  prober.io.mshr_rdy := mshrs.io.probe_rdy

  // refills
  def doRefill(g: Grant): Bool = co.messageUpdatesDataArray(g)
  val refill = if(refillCycles > 1) {
    val ser = Module(new FlowThroughSerializer(io.mem.grant.bits, refillCycles, doRefill))
    ser.io.in <> io.mem.grant
    ser.io.out
  } else io.mem.grant
  mshrs.io.mem_grant.valid := refill.fire()
  mshrs.io.mem_grant.bits := refill.bits
  refill.ready := writeArb.io.in(1).ready || !doRefill(refill.bits.payload)
  writeArb.io.in(1).valid := refill.valid && doRefill(refill.bits.payload)
  writeArb.io.in(1).bits := mshrs.io.mem_resp
  writeArb.io.in(1).bits.wmask := SInt(-1)
  writeArb.io.in(1).bits.data := refill.bits.payload.data(encTaggedRowBits-1,0)
  readArb.io.out.ready := !refill.valid || refill.ready // insert bubble if refill gets blocked
  readArb.io.out <> data.io.read

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  prober.io.wb_req <> wbArb.io.in(0)
  mshrs.io.wb_req <> wbArb.io.in(1)
  wbArb.io.out <> wb.io.req
  wb.io.meta_read <> metaReadArb.io.in(3)
  wb.io.data_req <> readArb.io.in(2)
  wb.io.data_resp := s2_data_corrected.toBits
  releaseArb.io.in(0) <> wb.io.release

  // store->load bypassing
  val s4_valid = Reg(next=s3_valid, init=Bool(false))
  val s4_req = RegEnable(s3_req, s3_valid && metaReadArb.io.out.valid)
  val s4_req_data = RegEnable(s3_req_data, s3_valid && metaReadArb.io.out.valid)
  val bypasses = List(
    ((s2_valid_masked || s2_replay) && !s2_sc_fail, s2_req, amoalu.io.out),
    (s3_valid, s3_req, s3_req_data),
    (s4_valid, s4_req, s4_req_data)
  ).map(r => (r._1 && (s1_addr >> wordOffBits === r._2.addr >> wordOffBits) && isWrite(r._2.cmd), r._3))
  val s2_store_bypass_data = Reg(Bits(width = taggedDataBits))
  val s2_store_bypass = Reg(Bool())
  when (s1_clk_en) {
    s2_store_bypass := false
    when (bypasses.map(_._1).reduce(_||_)) {
      s2_store_bypass_data := PriorityMux(bypasses)
      s2_store_bypass := true
    }
  }

  // load data subword mux/sign extension
  val s2_data_word_prebypass = s2_data_uncorrected(s2_word_idx)
  val s2_data_word = Mux(s2_store_bypass, s2_store_bypass_data, s2_data_word_prebypass)
  val loadgen = new LoadGen(s2_req.typ, s2_req.addr, s2_data_word, s2_sc, memTagBits)

  amoalu.io := s2_req
  amoalu.io.lhs := s2_data_word
  amoalu.io.rhs := s2_req.data

  // nack it like it's hot
  val s1_nack = dtlb.io.req.valid && dtlb.io.resp.miss ||
                s1_req.addr(indexmsb,indexlsb) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s2_nack_hit = RegEnable(s1_nack, s1_valid || s1_replay)
  when (s2_nack_hit) { mshrs.io.req.valid := Bool(false) }
  val s2_nack_victim = s2_hit && mshrs.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshrs.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_victim || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack

  val s2_recycle_ecc = (s2_valid || s2_replay) && s2_hit && s2_data_correctable
  val s2_recycle_next = Reg(init=Bool(false))
  when (s1_valid || s1_replay) { s2_recycle_next := (s1_valid || s1_replay) && s2_recycle_ecc }
  s2_recycle := s2_recycle_ecc || s2_recycle_next

  // after a nack, block until nack condition resolves to save energy
  val block_miss = Reg(init=Bool(false))
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_miss) {
    io.cpu.req.ready := Bool(false)
  }

  io.cpu.resp.valid  := (s2_replay || s2_valid_masked && s2_hit) && !s2_data_correctable
  io.cpu.resp.bits.nack := s2_valid && s2_nack
  io.cpu.resp.bits := s2_req
  io.cpu.resp.bits.has_data := isRead(s2_req.cmd) || s2_sc
  io.cpu.resp.bits.replay := s2_replay
  io.cpu.resp.bits.data := loadgen.word
  io.cpu.resp.bits.data_subword := loadgen.byte | s2_sc_fail
  io.cpu.resp.bits.store_data := s2_req.data
  io.cpu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid

  io.cpu.replay_next.valid := s1_replay && (s1_read || s1_sc)
  io.cpu.replay_next.bits := s1_req.tag

  io.mem.finish <> mshrs.io.mem_finish
}

// exposes a sane decoupled request interface
class SimpleHellaCacheIF extends Module
{
  val io = new Bundle {
    val requestor = new HellaCacheIO().flip
    val cache = new HellaCacheIO
  }

  val replaying_cmb = Bool()
  val replaying = Reg(next = replaying_cmb, init = Bool(false))
  replaying_cmb := replaying

  val replayq1 = Module(new Queue(new HellaCacheReq, 1, flow = true))
  val replayq2 = Module(new Queue(new HellaCacheReq, 1))
  val req_arb = Module(new Arbiter(new HellaCacheReq, 2))

  req_arb.io.in(0) <> replayq1.io.deq
  req_arb.io.in(1).valid := !replaying_cmb && io.requestor.req.valid
  req_arb.io.in(1).bits := io.requestor.req.bits
  io.requestor.req.ready := !replaying_cmb && req_arb.io.in(1).ready

  val s2_nack = io.cache.resp.bits.nack
  val s3_nack = Reg(next=s2_nack)

  val s0_req_fire = io.cache.req.fire()
  val s1_req_fire = Reg(next=s0_req_fire)
  val s2_req_fire = Reg(next=s1_req_fire)

  io.cache.req.bits.kill := s2_nack
  io.cache.req.bits.phys := Bool(true)
  io.cache.req.bits.data := RegEnable(req_arb.io.out.bits.data, s0_req_fire)
  io.cache.req <> req_arb.io.out

  // replay queues
  // replayq1 holds the older request
  // replayq2 holds the newer request (for the first nack)
  // we need to split the queues like this for the case where the older request
  // goes through but gets nacked, while the newer request stalls
  // if this happens, the newer request will go through before the older
  // request
  // we don't need to check replayq1.io.enq.ready and replayq2.io.enq.ready as
  // there will only be two requests going through at most

  // stash d$ request in stage 2 if nacked (older request)
  replayq1.io.enq.valid := Bool(false)
  replayq1.io.enq.bits.cmd := io.cache.resp.bits.cmd
  replayq1.io.enq.bits.typ := io.cache.resp.bits.typ
  replayq1.io.enq.bits.addr := io.cache.resp.bits.addr
  replayq1.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq1.io.enq.bits.tag := io.cache.resp.bits.tag

  // stash d$ request in stage 1 if nacked (newer request)
  replayq2.io.enq.valid := s2_req_fire && s3_nack
  replayq2.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq2.io.enq.bits <> io.cache.resp.bits
  replayq2.io.deq.ready := Bool(false)

  when (s2_nack) {
    replayq1.io.enq.valid := Bool(true)
    replaying_cmb := Bool(true)
  }

  // when replaying request got sunk into the d$
  when (s2_req_fire && Reg(next=Reg(next=replaying_cmb)) && !s2_nack) {
    // see if there's a stashed request in replayq2
    when (replayq2.io.deq.valid) {
      replayq1.io.enq.valid := Bool(true)
      replayq1.io.enq.bits.cmd := replayq2.io.deq.bits.cmd
      replayq1.io.enq.bits.typ := replayq2.io.deq.bits.typ
      replayq1.io.enq.bits.addr := replayq2.io.deq.bits.addr
      replayq1.io.enq.bits.data := replayq2.io.deq.bits.data
      replayq1.io.enq.bits.tag := replayq2.io.deq.bits.tag
      replayq2.io.deq.ready := Bool(true)
    } .otherwise {
      replaying_cmb := Bool(false)
    }
  }

  io.requestor.resp := io.cache.resp
}
