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
  }
}

class RocketTile(id: Int = 0) extends Tile {
  val icache = Module(new Frontend, { case CacheName => "L1I"; case CoreName => "Rocket" })
  val dcache = Module(new HellaCache, { case CacheName => "L1D" })
  val ptw = Module(new PTW(params(NPTWPorts)))
  val core = Module(new Core, { case CoreName => "Rocket" })

  dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr // Bypass signal to dcache
  val dcArb = Module(new HellaCacheArbiter(params(NDCachePorts)))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
  dcArb.io.mem <> dcache.io.cpu

  ptw.io.requestor(0) <> icache.io.ptw
  ptw.io.requestor(1) <> dcache.io.ptw

  core.io.imem <> icache.io.cpu
  core.io.ptw <> ptw.io.dpath

  core.io.io <> dcache.io.io

  // Connect the caches and ROCC to the outer memory system
  io.cached <> dcache.io.mem
  // If so specified, build an RoCC module and wire it in
  // otherwise, just hookup the icache
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
