package rocket

import Chisel._;
import Node._;
import Constants._;
import hwacha._

class ioDebug(view: List[String] = null) extends Bundle(view)
{
  val error_mode  = Bool(OUTPUT);
}

class ioRocket extends Bundle()
{
  val debug   = new ioDebug();
  val host    = new ioHTIF();
  val imem    = new ioImem().flip();
  val vimem   = new ioImem().flip();
  val dmem    = new ioDmem().flip();
}

class rocketProc(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io    = new ioRocket();
   
  val ctrl  = new rocketCtrl();      
  val dpath = new rocketDpath();

  val dtlb  = new rocketDTLB(DTLB_ENTRIES);
  val itlb  = new rocketITLB(ITLB_ENTRIES);
  val vitlb = new rocketITLB(ITLB_ENTRIES);
  val ptw   = new rocketPTW();
  val arb   = new rocketDmemArbiter();

  var vu: vu = null
  if (HAVE_VEC)
  {
    vu = new vu()
    // cpu, vector prefetch, and vector use the DTLB
    val dtlbarb = new cArbiter(3)({new ioDTLB_CPU_req()})
    val dtlbchosen = Reg(resetVal=Bits(DTLB_CPU,log2up(3)))
    when( dtlb.io.cpu_req.ready && dtlbarb.io.out.valid ) { dtlbchosen := dtlbarb.io.chosen }

    val chosen_vec = dtlbchosen === Bits(DTLB_VEC)
    val chosen_pf = dtlbchosen === Bits(DTLB_VPF)
    val chosen_cpu = dtlbchosen === Bits(DTLB_CPU)

    // vector prefetch doesn't care about exceptions
    // and shouldn't cause any anyways
    vu.io.vec_tlb_resp.xcpt_ld := chosen_vec && dtlb.io.cpu_resp.xcpt_ld
    vu.io.vec_tlb_resp.xcpt_st := chosen_vec && dtlb.io.cpu_resp.xcpt_st
    vu.io.vec_tlb_resp.miss := chosen_vec && dtlb.io.cpu_resp.miss
    vu.io.vec_tlb_resp.ppn := dtlb.io.cpu_resp.ppn

    vu.io.vec_pftlb_resp.xcpt_ld := Bool(false)
    vu.io.vec_pftlb_resp.xcpt_st := Bool(false)
    vu.io.vec_pftlb_resp.miss := chosen_pf && dtlb.io.cpu_resp.miss
    vu.io.vec_pftlb_resp.ppn := dtlb.io.cpu_resp.ppn

    // connect DTLB to ctrl+dpath
    dtlbarb.io.in(DTLB_CPU).valid := ctrl.io.dtlb_val
    dtlbarb.io.in(DTLB_CPU).bits.kill := ctrl.io.dtlb_kill
    dtlbarb.io.in(DTLB_CPU).bits.cmd := ctrl.io.dmem.req_cmd
    dtlbarb.io.in(DTLB_CPU).bits.asid := Bits(0,ASID_BITS); // FIXME: connect to PCR
    dtlbarb.io.in(DTLB_CPU).bits.vpn := dpath.io.dmem.req_addr(VADDR_BITS,PGIDX_BITS)
    ctrl.io.dtlb_rdy := dtlbarb.io.in(DTLB_CPU).ready

    ctrl.io.xcpt_dtlb_ld := chosen_cpu && dtlb.io.cpu_resp.xcpt_ld
    ctrl.io.xcpt_dtlb_st := chosen_cpu && dtlb.io.cpu_resp.xcpt_st
    ctrl.io.dtlb_miss := chosen_cpu && dtlb.io.cpu_resp.miss

    dtlbarb.io.in(DTLB_VEC) <> vu.io.vec_tlb_req
    dtlbarb.io.in(DTLB_VPF) <> vu.io.vec_pftlb_req


    dtlb.io.cpu_req <> dtlbarb.io.out
  }
  else
  {
    // connect DTLB to ctrl+dpath
    dtlb.io.cpu_req.valid := ctrl.io.dtlb_val
    dtlb.io.cpu_req.bits.kill := ctrl.io.dtlb_kill
    dtlb.io.cpu_req.bits.cmd := ctrl.io.dmem.req_cmd
    dtlb.io.cpu_req.bits.asid := Bits(0,ASID_BITS); // FIXME: connect to PCR
    dtlb.io.cpu_req.bits.vpn := dpath.io.dmem.req_addr(VADDR_BITS,PGIDX_BITS)
    ctrl.io.xcpt_dtlb_ld := dtlb.io.cpu_resp.xcpt_ld
    ctrl.io.xcpt_dtlb_st := dtlb.io.cpu_resp.xcpt_st
    ctrl.io.dtlb_rdy := dtlb.io.cpu_req.ready
    ctrl.io.dtlb_miss := dtlb.io.cpu_resp.miss
  }

  dtlb.io.invalidate := dpath.io.ptbr_wen
  dtlb.io.status := dpath.io.ctrl.status

  arb.io.cpu.req_ppn := dtlb.io.cpu_resp.ppn;
  ctrl.io.dmem.req_rdy := dtlb.io.cpu_req.ready && arb.io.cpu.req_rdy;

  // connect DTLB to D$ arbiter
  ctrl.io.xcpt_ma_ld := io.dmem.xcpt_ma_ld
  ctrl.io.xcpt_ma_st := io.dmem.xcpt_ma_st
  // connect page table walker to TLBs, page table base register (from PCR)
  // and D$ arbiter (selects between requests from pipeline and PTW, PTW has priority)
  ptw.io.dtlb             <> dtlb.io.ptw;
  ptw.io.itlb             <> itlb.io.ptw;
  ptw.io.ptbr             := dpath.io.ptbr;
  arb.io.ptw              <> ptw.io.dmem;
  arb.io.mem              <> io.dmem

  ctrl.io.dpath             <> dpath.io.ctrl;
  dpath.io.host             <> io.host;
  dpath.io.debug            <> io.debug;

  // FIXME: try to make this more compact
  
  // connect ITLB to I$, ctrl, dpath
  itlb.io.cpu.invalidate  := dpath.io.ptbr_wen;
  itlb.io.cpu.status      := dpath.io.ctrl.status;
  itlb.io.cpu.req_val     := ctrl.io.imem.req_val;  
  itlb.io.cpu.req_asid    := Bits(0,ASID_BITS); // FIXME: connect to PCR
  itlb.io.cpu.req_vpn     := dpath.io.imem.req_addr(VADDR_BITS,PGIDX_BITS);
  io.imem.req_idx         := dpath.io.imem.req_addr(PGIDX_BITS-1,0);
  io.imem.req_ppn         := itlb.io.cpu.resp_ppn;
  io.imem.req_val         := ctrl.io.imem.req_val;
  io.imem.invalidate      := ctrl.io.dpath.flush_inst;
  ctrl.io.imem.resp_val   := io.imem.resp_val;
  dpath.io.imem.resp_data := io.imem.resp_data;
  ctrl.io.xcpt_itlb       := itlb.io.cpu.exception;
  io.imem.itlb_miss       := itlb.io.cpu.resp_miss;

  // connect arbiter to ctrl+dpath+DTLB
  arb.io.cpu.req_val      := ctrl.io.dmem.req_val;
  arb.io.cpu.req_cmd      := ctrl.io.dmem.req_cmd;
  arb.io.cpu.req_type     := ctrl.io.dmem.req_type;
  arb.io.cpu.req_kill     := ctrl.io.dmem.req_kill;
  arb.io.cpu.req_idx      := dpath.io.dmem.req_addr(PGIDX_BITS-1,0);
  arb.io.cpu.req_data     := dpath.io.dmem.req_data;
  arb.io.cpu.req_tag      := dpath.io.dmem.req_tag;
  ctrl.io.dmem.resp_miss  := arb.io.cpu.resp_miss;
  ctrl.io.dmem.resp_replay:= arb.io.cpu.resp_replay;
  ctrl.io.dmem.resp_nack  := arb.io.cpu.resp_nack;
  dpath.io.dmem.resp_val  := arb.io.cpu.resp_val;
  dpath.io.dmem.resp_miss := arb.io.cpu.resp_miss;
  dpath.io.dmem.resp_replay := arb.io.cpu.resp_replay;
  dpath.io.dmem.resp_type := io.dmem.resp_type;
  dpath.io.dmem.resp_tag  := arb.io.cpu.resp_tag;
  dpath.io.dmem.resp_data := arb.io.cpu.resp_data;  
  dpath.io.dmem.resp_data_subword := io.dmem.resp_data_subword;

  var fpu: rocketFPU = null
  if (HAVE_FPU)
  {
    fpu = new rocketFPU(4,6)
    dpath.io.fpu <> fpu.io.dpath
    ctrl.io.fpu <> fpu.io.ctrl
  }
  else
    ctrl.io.fpu.dec.valid := Bool(false)

  if (HAVE_VEC)
  {
    dpath.io.vec_ctrl <> ctrl.io.vec_dpath

    // hooking up vector I$
    vitlb.io.cpu.invalidate := dpath.io.ptbr_wen
    vitlb.io.cpu.status     := dpath.io.ctrl.status
    vitlb.io.cpu.req_val    := vu.io.imem_req.valid  
    vitlb.io.cpu.req_asid   := Bits(0,ASID_BITS) // FIXME: connect to PCR
    vitlb.io.cpu.req_vpn    := vu.io.imem_req.bits(VADDR_BITS,PGIDX_BITS).toUFix
    io.vimem.req_idx        := vu.io.imem_req.bits(PGIDX_BITS-1,0)
    io.vimem.req_ppn        := vitlb.io.cpu.resp_ppn
    io.vimem.req_val        := vu.io.imem_req.valid
    io.vimem.invalidate     := ctrl.io.dpath.flush_inst
    vu.io.imem_req.ready    := Bool(true)
    vu.io.imem_resp.valid   := io.vimem.resp_val
    vu.io.imem_resp.bits    := io.vimem.resp_data
    // handle vitlb.io.cpu.exception
    io.vimem.itlb_miss      := vitlb.io.cpu.resp_miss

    // hooking up vector command queues
    vu.io.vec_cmdq.valid := ctrl.io.vec_iface.vcmdq_valid
    vu.io.vec_cmdq.bits := dpath.io.vec_iface.vcmdq_bits
    vu.io.vec_ximm1q.valid := ctrl.io.vec_iface.vximm1q_valid
    vu.io.vec_ximm1q.bits := dpath.io.vec_iface.vximm1q_bits
    vu.io.vec_ximm2q.valid := ctrl.io.vec_iface.vximm2q_valid
    vu.io.vec_ximm2q.bits := dpath.io.vec_iface.vximm2q_bits

    // prefetch queues
    vu.io.vec_pfcmdq.valid := ctrl.io.vec_iface.vpfcmdq_valid
    vu.io.vec_pfcmdq.bits := dpath.io.vec_iface.vcmdq_bits
    vu.io.vec_pfximm1q.valid := ctrl.io.vec_iface.vpfximm1q_valid
    vu.io.vec_pfximm1q.bits := dpath.io.vec_iface.vximm1q_bits
    vu.io.vec_pfximm2q.valid := ctrl.io.vec_iface.vpfximm2q_valid
    vu.io.vec_pfximm2q.bits := dpath.io.vec_iface.vximm2q_bits

    // don't have to use pf ready signals
    // if cmdq is not a load or store
    ctrl.io.vec_iface.vcmdq_ready := vu.io.vec_cmdq.ready
    ctrl.io.vec_iface.vximm1q_ready := vu.io.vec_ximm1q.ready
    ctrl.io.vec_iface.vximm2q_ready := vu.io.vec_ximm2q.ready
    ctrl.io.vec_iface.vpfcmdq_ready := vu.io.vec_pfcmdq.ready
    ctrl.io.vec_iface.vpfximm1q_ready := vu.io.vec_pfximm1q.ready
    ctrl.io.vec_iface.vpfximm2q_ready := vu.io.vec_pfximm2q.ready
    ctrl.io.vec_iface.vackq_valid := vu.io.vec_ackq.valid
    vu.io.vec_ackq.ready := ctrl.io.vec_iface.vackq_ready

    // exceptions
    // dpath.io.vec_iface.eaddr
    // dpath.io.vec_iface.exception

    // hooking up vector memory interface
    ctrl.io.ext_mem.req_val := vu.io.dmem_req.valid
    ctrl.io.ext_mem.req_cmd := vu.io.dmem_req.bits.cmd
    ctrl.io.ext_mem.req_type := vu.io.dmem_req.bits.typ

    dpath.io.ext_mem.req_val := vu.io.dmem_req.valid
    dpath.io.ext_mem.req_idx := vu.io.dmem_req.bits.idx
    dpath.io.ext_mem.req_ppn := vu.io.dmem_req.bits.ppn
    dpath.io.ext_mem.req_data := vu.io.dmem_req.bits.data
    dpath.io.ext_mem.req_tag := vu.io.dmem_req.bits.tag

    vu.io.dmem_resp.valid := dpath.io.ext_mem.resp_val
    vu.io.dmem_resp.bits.nack := ctrl.io.ext_mem.resp_nack
    vu.io.dmem_resp.bits.data := dpath.io.ext_mem.resp_data
    vu.io.dmem_resp.bits.tag := dpath.io.ext_mem.resp_tag
    vu.io.dmem_resp.bits.typ := dpath.io.ext_mem.resp_type

    // share vector integer multiplier with rocket
    dpath.io.vec_imul_req <> vu.io.cp_imul_req
    dpath.io.vec_imul_resp <> vu.io.cp_imul_resp

    fpu.io.sfma.valid := Bool(false)
    fpu.io.dfma.valid := Bool(false)
  }
  else
  {
    ctrl.io.ext_mem.req_val := Bool(false)
    dpath.io.ext_mem.req_val := Bool(false)

    if (HAVE_FPU)
    {
      fpu.io.sfma.valid := Bool(false)
      fpu.io.dfma.valid := Bool(false)
    }
  }
}
