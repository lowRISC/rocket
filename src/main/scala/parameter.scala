// See LICENSE for license details.

package rocket
import Chisel._

//Tile Constants
case object NTiles extends Field[Int]
case object CoreName extends Field[String] // only useful when Hwacha is used
case object NDCachePorts extends Field[Int]
case object NPTWPorts extends Field[Int]
case object BuildRoCC extends Field[Option[() => RoCC]]

// Rocket Core Constants
case object FetchWidth extends Field[Int]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object FastMulDiv extends Field[Boolean]
case object NMultXpr extends Field[Int]
case object BuildFPU extends Field[Option[() => FPU]]
case object FDivSqrt extends Field[Boolean]
case object CoreInstBits extends Field[Int]
case object CoreDCacheReqTagBits extends Field[Int]
case object NCustomMRWCSRs extends Field[Int]
