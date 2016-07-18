/*package lms

import scala.lms.common._
import scala.lms.internal._

/**
 * @author Sandro Stucki, Manohar Jonnalagedda
 *
 * Generate a "zero" value for a given type.
 *
 * The apply method of this object can be used to generate a constant
 * "zero" value for a given type `T` like so:
 *
 * {{{
 *   val zero = zeroVal[T]
 * }}}
 *
 * The return value can be used e.g. to initialize variables.  This is
 * useful when one needs to generate constants symbols in a generic
 * way, e.g. when initializing variables or fields of structs.  The
 * special treatment of value types ensures the correct behavior
 * e.g. when generating a string literal from a constant symbol.
 *
 * Update: implementation changed after the `Manifest` -> `Typ` migration
 * As `Typ` is defined in `Expressions`, `ZeroVal` needs to be part of the cake
 *
 * We mix in Primitive and Bool ops to inherit their implicit proofs of Typ[Int]
 * and Typ[Bool]
 */

trait ZeroVal
    extends Base
    with PrimitiveOps
    with BooleanOps {

  type Nul[T]
  def zeroVal[A: Typ: Nul]: Rep[A]

  implicit def intNul: Nul[Int]
  implicit def boolNul: Nul[Boolean]
}

trait ZeroValExp
    extends ZeroVal
    with Expressions
    with PrimitiveOpsExp
    with BooleanOpsExp {

  abstract class Nul[A: Typ] {
    def nullValue: Rep[A]
  }

  def zeroVal[A: Typ: Nul]: Rep[A] = implicitly[Nul[A]].nullValue

  implicit object IntNul extends Nul[Int] {
    def nullValue = unit(0)
  }

  implicit object BooleanNul extends Nul[Boolean] {
    def nullValue = unit(false)
  }

  implicit def intNul: Nul[Int] = IntNul
  implicit def boolNul: Nul[Boolean] = BooleanNul


//  implicit object ByteNul extends Nul[Byte] {
//    def nullValue = unit(0.toByte)
//  }
//
//  implicit object CharNul extends Nul[Char] {
//    def nullValue = unit(0.toChar)
//  }
//
//  implicit object LongNul extends Nul[Long] {
//    def nullValue = unit(0L)
//  }
//
//  implicit object ShortNul extends Nul[Short] {
//    def nullValue = unit(0.toShort)
//  }
//
//  implicit object DoubleNul extends Nul[Double] {
//    def nullValue = unit(0.0)
//  }
//
//  implicit object FloatNul extends Nul[Float] {
//    def nullValue = unit((0.0).toFloat)
//  }
//
//  implicit object UnitNul extends Nul[Unit] {
//    def nullValue = unit(())
//  }
}
*/
