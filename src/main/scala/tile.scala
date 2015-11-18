// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._

abstract class Tile extends Module {
  val io = new Bundle {
    val cached = new ClientTileLinkIO
    val uncached = new ClientUncachedTileLinkIO
    val io = new ClientUncachedTileLinkIO
    val pcr = new PCRIO
    val irq = Bool(INPUT)
    val soft_reset = Bool(INPUT)
  }
}

class RocketTile(id: Int = 0) extends Tile {

  val soft_reset = reset || io.soft_reset

  val icache = Module(new Frontend(resetSignal = soft_reset), { case CacheName => "L1I"; case CoreName => "Rocket" })
  val dcache = Module(new HellaCache(resetSignal = soft_reset), { case CacheName => "L1D" })
  val ptw = Module(new PTW(n = params(NPTWPorts), resetSignal = soft_reset))
  val core = Module(new Rocket(id = id, resetSignal = soft_reset), { case CoreName => "Rocket" })

  dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr // Bypass signal to dcache
  val dcArb = Module(new HellaCacheArbiter(params(NDCachePorts)))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcache.io.cpu <> dcArb.io.mem
  dcache.io.pcr_update := io.pcr.update

  ptw.io.requestor(0) <> icache.io.ptw
  ptw.io.requestor(1) <> dcache.io.ptw

  io.io <> dcache.io.io
  icache.io.cpu <> core.io.imem
  core.io.ptw <> ptw.io.dpath
  core.io.pcr <> io.pcr
  core.io.irq <> io.irq

  //If so specified, build an FPU module and wire it in
  if(params(BuildFPU)) {
    val fpu = Module(new FPU(soft_reset))
    core.io.fpu <> fpu.io
  }

  // Connect the caches and ROCC to the outer memory system
  io.cached <> dcache.io.mem
  // If so specified, build an RoCC module and wire it in
  // otherwise, just hookup the icache
  // ToDo: add soft_reset if enabled
  io.uncached <> params(BuildRoCC).map { buildItHere =>
    val rocc = buildItHere()
    val memArb = Module(new ClientTileLinkIOArbiter(3))
    val dcIF = Module(new SimpleHellaCacheIF)
    core.io.rocc <> rocc.io
    dcIF.io.requestor <> rocc.io.mem
    dcArb.io.requestor(2) <> dcIF.io.cache
    memArb.io.in(0) <> icache.io.mem
    memArb.io.in(1) <> rocc.io.imem
    memArb.io.in(2) <> rocc.io.dmem
    ptw.io.requestor(2) <> rocc.io.iptw
    ptw.io.requestor(3) <> rocc.io.dptw
    ptw.io.requestor(4) <> rocc.io.pptw
    memArb.io.out
  }.getOrElse(icache.io.mem)
}
