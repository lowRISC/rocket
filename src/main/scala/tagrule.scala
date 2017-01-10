// See LICENSE.Cambridge for license details

package rocket

import Chisel._
import cde.{Parameters, Field}
import scala.math.max

case object TagRuleALUSize extends Field[Int]
case object TagRuleMemSize extends Field[Int]

trait TagOpConstants {
  val TG_OP_SZ      = 3
  val TG_OP_X       = BitPat("b???")
  val TG_OP_NONE    = UInt(0, TG_OP_SZ)
  val TG_OP_ALU     = UInt(1, TG_OP_SZ)
  val TG_OP_MEM     = UInt(2, TG_OP_SZ)
}


class TagRuleReq(implicit p: Parameters) extends CoreBundle()(p) {
  val tg_op = UInt(width=TG_OP_SZ)   // from ID
  val instr_tag = UInt(width=tgBits) // from ID
  val rs1_tag = UInt(width=tgBits) // from EX
  val rs2_tag = UInt(width=tgBits) // from EX
}

class TagCheck(implicit p: Parameters) extends CoreBundle()(p) {
  val checkL = UInt(width=tgBits)
  val checkR = UInt(width=tgBits)
}

class TagRuleIO(implicit p: Parameters) extends CoreBundle()(p) {
  val cpu_req = Decoupled(new TagRuleReq).flip
  val cpu_tag = Valid(UInt(width=tgBits)) // update tag for rd
  val dmem_check = Valid(new TagCheck)    // tag check for D$
  val miss = Bool(OUTPUT)                 // miss in tag rule table
  val xcpt = Bool(OUTPUT)                 // exception raised
}

trait TgHashOP {
  val HASH_OP_SZ    = 3
  val HASH_NONE     = UInt(0, HASH_OP_SZ) // rd keeps the old tag
  val HASH_ZERO     = UInt(1, HASH_OP_SZ) // rd_t <= 0
  val HASH_RS1      = UInt(2, HASH_OP_SZ) // rd_t <= rs1_t
  val HASH_RS2      = UInt(3, HASH_OP_SZ) // rd_t <= rs2_t
  val HASH_ADR_RS1  = UInt(4, HASH_OP_SZ) // addr <= base | rs1_t
  val HASH_ADR_INST = UInt(5, HASH_OP_SZ) // addr <= base | inst_t
  val HASH_ADR_RS   = UInt(6, HASH_OP_SZ) // addr <= base | {rs2_t,rs1_t}
  val HASH_ADR_ALL  = UInt(7, HASH_OP_SZ) // addr <= base | {inst_t, rs2_t, rs1_t}
}

class TagRule(implicit p: Parameters) extends CoreModule()(p) with TgHashOP {

  val ruleTableALUSize = p(TagRuleALUSize)
  val ruleTableALUAddrBits = log2Up(ruleTableALUSize)
  val ruleTableALUEntryBits = tgBits+1 // [td_tag,xcpt]
  val ruleTableMemSize = p(TagRuleMemSize)
  val ruleTableMemAddrBits = log2Up(ruleTableMemSize)
  val ruleTableMemEntryBits = tgBits * 2 // [checkR,checkL]
  val ruleTableSize = ruleTableALUSize + ruleTableMemSize
  val ruleTableAddrBits = log2Up(ruleTableSize)
  val hashTableSize = 1 << TG_OP_SZ
  val hashTableEntryBits =  HASH_OP_SZ + ruleTableAddrBits
  require(isPow2(ruleTableALUSize))

  val io = new TagRuleIO

  // the address hash table
  val hashTable = SeqMem(hashTableSize, Vec(1, UInt(width=hashTableEntryBits)))
  val hashTableContent = Vec(
      Cat(UInt(0, ruleTableAddrBits), HASH_ZERO),       // TG_OP_NONE: reset
      Cat(UInt(0, ruleTableAddrBits), HASH_RS1),        // TG_OP_ALU: rd_t <= rs1_t
      Cat(UInt(0, ruleTableAddrBits), HASH_RS1)         // TG_OP_MEM: ? mem_t == rs1_t
    )

  // the tag rule table
  val ruleTableALU = SeqMem(ruleTableALUSize, Vec(1, UInt(width=ruleTableALUEntryBits)))
  val ruleTableMem = SeqMem(ruleTableMemSize, Vec(1, UInt(width=ruleTableMemEntryBits)))

  // reset all tables
  val rstCycles = Seq(hashTableSize, ruleTableALUSize, ruleTableMemSize).reduce(_ max _)
  val rstCnt = Reg(init=UInt(0, rstCycles+1))
  val rstDone = rstCnt === UInt(rstCycles)
  rstCnt := Mux(!rstDone, rstCnt+UInt(1), rstCnt)
  io.cpu_req.ready := rstDone

  when(rstCnt < UInt(hashTableSize)) {
    val rstVal = Mux(rstCnt < UInt(hashTableContent.size),
                     hashTableContent(rstCnt), UInt(0))
    hashTable.write(rstCnt, Vec.fill(1)(rstVal), UInt(1))
  }

  when(rstCnt < UInt(ruleTableALUSize)) {
    ruleTableALU.write(rstCnt, Vec.fill(1)(UInt(0)), UInt(1))
  }

  when(rstCnt < UInt(ruleTableMemSize)) {
    ruleTableMem.write(rstCnt, Vec.fill(1)(UInt(0)), UInt(1))
  }

  // decoder stage
  val id_valid = io.cpu_req.fire()

  // execution stage
  val ex_valid = Reg(next=id_valid)
  val ex_rddata = hashTable.read(io.cpu_req.bits.tg_op, id_valid).toBits
  val ex_hash = ex_rddata(HASH_OP_SZ-1,0)
  val ex_instr_tag = RegEnable(io.cpu_req.bits.instr_tag, id_valid)
  val ex_rs1_tag = io.cpu_req.bits.rs1_tag
  val ex_rs2_tag = io.cpu_req.bits.rs2_tag
  val ex_rd_tag = Wire(init = UInt(0, tgBits))
  switch(ex_hash) {
    is (HASH_ZERO ) { ex_rd_tag := UInt(0)    }
    is (HASH_RS1  ) { ex_rd_tag := ex_rs1_tag }
    is (HASH_RS2  ) { ex_rd_tag := ex_rs2_tag }
  }
  val ex_read_table = ex_hash(2) // change when TgHashOp changed
  val ex_base = ex_rddata >> HASH_OP_SZ
  val ex_offset = Wire(init = UInt(0, ruleTableAddrBits))
  switch(ex_hash) {
    is (HASH_ADR_RS1  ) { ex_offset := ex_rs1_tag                                }
    is (HASH_ADR_INST ) { ex_offset := ex_instr_tag                              }
    is (HASH_ADR_RS   ) { ex_offset := Cat(ex_rs2_tag, ex_rs1_tag)               }
    is (HASH_ADR_ALL  ) { ex_offset := Cat(ex_instr_tag, ex_rs2_tag, ex_rs1_tag) }
  }
  val ex_addr_alu = ex_base | ex_offset
  val ex_addr_mem = ex_addr_alu ^ UInt(ruleTableALUSize)
  val ex_alu_mem = ex_base < UInt(ruleTableALUSize) // use base address to identify which table to read

  // assertion to make sure rs2_t is not used to generate a memory check
  assert(ex_alu_mem || !(ex_hash === HASH_ADR_RS || ex_hash === HASH_ADR_ALL),
         "rs2_tag cannot be used to address a memory check function!")

  // memory stage
  val mem_valid = Reg(next=ex_valid)
  val mem_read_table = RegEnable(ex_read_table, mem_valid)
  val mem_alu_mem = Reg(next=ex_alu_mem)
  val mem_alu_rdata = ruleTableALU.read(ex_addr_alu, ex_valid && ex_alu_mem).toBits
  val mem_mem_rdata = ruleTableMem.read(ex_addr_mem, ex_valid && !ex_alu_mem).toBits
  val mem_rd_tag = Mux(mem_read_table, mem_alu_rdata(tgBits,1), RegEnable(ex_rd_tag, !ex_read_table))
  val mem_rd_update = Reg(next = ex_hash =/= HASH_NONE)
  val mem_checkL = Mux(mem_read_table && !mem_alu_mem, mem_mem_rdata(tgBits-1,0), mem_rd_tag)
  val mem_checkR = Mux(mem_read_table && !mem_alu_mem, mem_mem_rdata(2*tgBits-1,tgBits), UInt(0))

  // writeback stage
  val wb_rd_update = Reg(next = mem_valid && mem_rd_update)
  val wb_xcpt = RegNext(mem_alu_rdata(0), mem_valid && mem_read_table && mem_alu_mem)
  val wb_rd_tag = RegNext(mem_rd_tag, mem_valid && mem_rd_update)
  val wb_check = Reg(next = mem_valid) // always issue check, if not needed, use HASH_ZERO
  val wb_checkL = RegEnable(mem_checkL, mem_valid)
  val wb_checkR = RegEnable(mem_checkR, mem_valid)

  // output ports
  io.cpu_tag.valid := wb_rd_update
  io.cpu_tag.bits := wb_rd_tag
  io.dmem_check.valid := wb_check
  io.dmem_check.bits.checkL := wb_checkL
  io.dmem_check.bits.checkR := wb_checkR
  io.miss := Bool(false) // if miss in the future checked rule table
  io.xcpt := wb_xcpt     // precise exception caused by a non-memory instruction's tag

}
