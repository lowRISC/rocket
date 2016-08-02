// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import open_soc_debug._
import Util._
import cde.{Parameters, Field}

case object CoreName extends Field[String]
case object BuildRoCC extends Field[Seq[RoccParameters]]

case class RoccParameters(
  opcodes: OpcodeSet,
  generator: Parameters => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  csrs: Seq[Int] = Nil,
  useFPU: Boolean = false)

abstract class Tile(resetSignal: Bool = null)
                   (implicit p: Parameters) extends Module(_reset = resetSignal) {
  val buildRocc = p(BuildRoCC)
  val usingRocc = !buildRocc.isEmpty
  val nRocc = buildRocc.size
  val nFPUPorts = buildRocc.filter(_.useFPU).size
  val nCachedTileLinkPorts = 1
  val nUncachedTileLinkPorts = 1 + p(RoccNMemChannels)
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })
  val io = new Bundle {
    val cached = Vec(nCachedTileLinkPorts, new ClientTileLinkIO)
    val uncached = Vec(nUncachedTileLinkPorts, new ClientUncachedTileLinkIO)

    val irq = Bool(INPUT)
    val dbgrst = Bool(INPUT)
    val dbgnet = Vec(2, new DiiIO)       // debug network
    val prci = new PRCITileIO().flip
    val dma = new DmaIO
  }
}

class RocketTile(id: Int = 0, resetSignal: Bool = null)(implicit p: Parameters) extends Tile(resetSignal)(p) {
  val core = Module(new Rocket(id)(p.alterPartial({ case CoreName => "Rocket" })))
  val icache = Module(new Frontend()(p.alterPartial({
    case CacheName => "L1I"
    case CoreName => "Rocket" })))
  val dcache = Module(new HellaCache()(dcacheParams))

  val ptwPorts = collection.mutable.ArrayBuffer(icache.io.ptw, dcache.io.ptw)
  val dcPorts = collection.mutable.ArrayBuffer(core.io.dmem)
  val uncachedArbPorts = collection.mutable.ArrayBuffer(icache.io.mem)
  val uncachedPorts = collection.mutable.ArrayBuffer[ClientUncachedTileLinkIO]()
  val cachedPorts = collection.mutable.ArrayBuffer(dcache.io.mem)
  core.io.prci <> io.prci
  icache.io.cpu <> core.io.imem
  core.io.irq <> io.irq

  val fpuOpt = if (p(UseFPU)) Some(Module(new FPU)) else None
  fpuOpt.foreach(fpu => core.io.fpu <> fpu.io)

  if (usingRocc) {
    val respArb = Module(new RRArbiter(new RoCCResponse, nRocc))
    core.io.rocc.resp <> respArb.io.out

    val roccOpcodes = buildRocc.map(_.opcodes)
    val cmdRouter = Module(new RoccCommandRouter(roccOpcodes))
    cmdRouter.io.in <> core.io.rocc.cmd

    val roccs = buildRocc.zipWithIndex.map { case (accelParams, i) =>
      val rocc = accelParams.generator(p.alterPartial({
        case RoccNMemChannels => accelParams.nMemChannels
        case RoccNPTWPorts => accelParams.nPTWPorts
        case RoccNCSRs => accelParams.csrs.size
      }))
      val dcIF = Module(new SimpleHellaCacheIF()(dcacheParams))
      rocc.io.cmd <> cmdRouter.io.out(i)
      rocc.io.status := core.io.rocc.status
      rocc.io.exception := core.io.rocc.exception
      rocc.io.host_id := UInt(id)
      dcIF.io.requestor <> rocc.io.mem
      dcPorts += dcIF.io.cache
      uncachedArbPorts += rocc.io.autl
      rocc
    }

    if (nFPUPorts > 0) {
      fpuOpt.foreach { fpu =>
        val fpArb = Module(new InOrderArbiter(new FPInput, new FPResult, nFPUPorts))
        val fp_roccs = roccs.zip(buildRocc)
          .filter { case (_, params) => params.useFPU }
          .map { case (rocc, _) => rocc.io }
        fpArb.io.in_req <> fp_roccs.map(_.fpu_req)
        fp_roccs.zip(fpArb.io.in_resp).foreach {
          case (rocc, fpu_resp) => rocc.fpu_resp <> fpu_resp
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      }
    }

    core.io.rocc.busy := cmdRouter.io.busy || roccs.map(_.io.busy).reduce(_ || _)
    core.io.rocc.interrupt := roccs.map(_.io.interrupt).reduce(_ || _)
    respArb.io.in <> roccs.map(rocc => Queue(rocc.io.resp))

    if (p(RoccNCSRs) > 0) {
      core.io.rocc.csr.rdata <> roccs.map(_.io.csr.rdata).reduce(_ ++ _)
      for ((rocc, accelParams) <- roccs.zip(buildRocc)) {
        rocc.io.csr.waddr := core.io.rocc.csr.waddr
        rocc.io.csr.wdata := core.io.rocc.csr.wdata
        rocc.io.csr.wen := core.io.rocc.csr.wen &&
          accelParams.csrs
            .map(core.io.rocc.csr.waddr === UInt(_))
            .reduce((a, b) => a || b)
      }
    }

    ptwPorts ++= roccs.flatMap(_.io.ptw)
    uncachedPorts ++= roccs.flatMap(_.io.utl)
  }

  val uncachedArb = Module(new ClientUncachedTileLinkIOArbiter(uncachedArbPorts.size))
  uncachedArb.io.in <> uncachedArbPorts
  uncachedArb.io.out +=: uncachedPorts

  // Connect the caches and RoCC to the outer memory system
  io.uncached <> uncachedPorts
  io.cached <> cachedPorts
  // TODO remove nCached/nUncachedTileLinkPorts parameters and these assertions
  require(uncachedPorts.size == nUncachedTileLinkPorts)
  require(cachedPorts.size == nCachedTileLinkPorts)

  if (p(UseVM)) {
    val ptw = Module(new PTW(ptwPorts.size)(dcacheParams))
    ptw.io.requestor <> ptwPorts
    ptw.io.mem +=: dcPorts
    core.io.ptw <> ptw.io.dpath
  }

  val dcArb = Module(new HellaCacheArbiter(dcPorts.size)(dcacheParams))
  dcArb.io.requestor <> dcPorts
  dcache.io.cpu <> dcArb.io.mem

  if (!usingRocc || nFPUPorts == 0) {
    fpuOpt.foreach { fpu =>
      fpu.io.cp_req.valid := Bool(false)
      fpu.io.cp_resp.ready := Bool(false)
    }
  }

  // debug
  io.dbgnet <> core.io.dbgnet
  io.dbgrst <> core.io.dbgrst
}
