// See LICENSE for license details

package rocket

import Chisel._
import Instructions._
import uncore.constants.MemoryOpConstants._
import ALU._
import cde.Parameters

abstract trait DecodeConstants extends HasCoreParameters
{
  val xpr64 = Bool(xLen == 64)

  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val mem_type = Bits(width = MT_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val tg_op = Bits(width = TG_OP_SZ)

  def default: List[BitPat] =
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | 
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | tg_op
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |  |
                List(N,    X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, X,X,X,X,X,X,CSR.X,X,X,X,TG_OP_X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd, mem_type,
                   rfs1, rfs2, rfs3, wfd, div, wxd, csr, fence_i, fence, amo, tg_op)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class XDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | 
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | |  tg_op
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |   |
    BNE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    BEQ->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    BLT->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    BLTU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    BGE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    BGEU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),

    JAL->       List(Y,    N,N,N,Y,N,N,N,A2_FOUR,A1_PC,  IMM_UJ,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    JALR->      List(Y,    N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    AUIPC->     List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),

    LB->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_B, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LH->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_H, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LW->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LD->        List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LBU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_BU,N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LHU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_HU,N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    LWU->       List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_WU,N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_MEM),
    SB->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_B, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_MEM),
    SH->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_H, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_MEM),
    SW->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_MEM),
    SD->        List(xpr64,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_MEM),

    AMOADD_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOXOR_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOSWAP_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOAND_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOOR_W->   List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMIN_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMINU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMAX_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMAXU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOADD_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOSWAP_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOXOR_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOAND_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOOR_D->   List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMIN_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMINU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMAX_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    AMOMAXU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),

    LR_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    LR_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    SC_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),
    SC_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y, TG_OP_MEM),

    LUI->       List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    ADDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLTI ->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLTIU->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    ANDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    ORI->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    XORI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRAI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    ADD->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SUB->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLT->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLTU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    AND->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    OR->        List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    XOR->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRA->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),

    ADDIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRAIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    ADDW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SUBW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SLLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),
    SRAW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_ALU),

    MUL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    MULH->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    MULHU->     List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    MULHSU->    List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    MULW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),

    DIV->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    DIVU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    REM->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    REMU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    DIVW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    DIVUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    REMW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),
    REMUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N, TG_OP_ALU),

    FENCE->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,Y,N, TG_OP_NONE),
    FENCE_I->   List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,Y,N,N, TG_OP_NONE),

    SFENCE_VM-> List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    SCALL->     List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    SBREAK->    List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    SRET->      List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    MRET->      List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    WFI->       List(Y,    N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.I,N,N,N, TG_OP_NONE),
    CSRRW->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N, TG_OP_NONE),
    CSRRS->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N, TG_OP_NONE),
    CSRRC->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N, TG_OP_NONE),
    CSRRWI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N, TG_OP_NONE),
    CSRRSI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N, TG_OP_NONE),
    CSRRCI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N, TG_OP_NONE))
}

class FDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           |
                //         fp_val| | renx2                                                         | | renf3         |
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | 
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | tg_op
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |   |
    FCVT_S_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_D_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJ_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJ_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJX_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJX_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJN_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSGNJN_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMIN_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMIN_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMAX_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMAX_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FADD_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FADD_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSUB_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSUB_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMUL_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMUL_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMADD_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMADD_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMSUB_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMSUB_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FNMADD_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FNMADD_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FNMSUB_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FNMSUB_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCLASS_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCLASS_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FMV_X_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FMV_X_D->   List(xpr64,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_W_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_W_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_WU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_WU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_L_S->  List(xpr64,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_L_D->  List(xpr64,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_LU_S-> List(xpr64,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_LU_D-> List(xpr64,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FEQ_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FEQ_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FLT_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FLT_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FLE_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FLE_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    FMV_S_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FMV_D_X->   List(xpr64,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_S_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_D_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_S_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_D_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_S_L->  List(xpr64,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_D_L->  List(xpr64,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_S_LU-> List(xpr64,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FCVT_D_LU-> List(xpr64,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FLW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FLD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,Y,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,Y,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE))
}

class FDivSqrtDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FDIV_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FDIV_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSQRT_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N, TG_OP_NONE),
    FSQRT_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N))
}

class RoCCDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        //               jal                                                               renf1             fence.i
                        //               | jalr                                                            | renf2           |
                        //         fp_val| | renx2                                                         | | renf3         |
                        //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | 
                        //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | fence
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | amo
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | |  tg_op
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | |   |
    CUSTOM0->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM0_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM0_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM0_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM0_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM0_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM1_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM2_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE),
    CUSTOM3_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N, TG_OP_NONE))
}
