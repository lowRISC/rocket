// See LICENSE.Cambridge for license details

package rocket

import Chisel._
import cde.Parameters

trait TagOpConstants {
  val TG_OP_SZ      = 3
  val TG_OP_X       = BitPat("b???")
  val TG_OP_NONE    = UInt(0, TG_OP_SZ)
  val TG_OP_MEM     = UInt(1, TG_OP_SZ)
}

