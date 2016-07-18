package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * Inspired from TupleOps in the delite-develop branch of LMS
 */

trait EitherOps
    extends Base
    with IfThenElse
    with BooleanOps
    with Equal
    /*with ZeroVal*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def either_typ[A: Typ, B: Typ]: Typ[Either[A, B]]
  implicit def either_nul[A: Typ: Nul, B: Typ: Nul]: Nul[Either[A, B]]

  implicit def make_either[A: Typ: Nul, B: Typ: Nul](o: Either[Rep[A], Rep[B]])(implicit pos: SourceContext): Rep[Either[A, B]]

  implicit class EitherOpsCls[A: Typ: Nul, B: Typ: Nul](o: Rep[Either[A, B]]) {

    /**
     * the "pattern" match
     */
    def isLeft: Rep[Boolean] = struct_isLeft(o)

    /**
     * map on Either
     */
    def map[C: Typ: Nul, D: Typ: Nul](l: Rep[A] => Rep[C], r: Rep[B] => Rep[D]): Rep[Either[C, D]] =
      if (o.isLeft) left[C, D](l(o.getLeft)) else right[C, D](r(o.getRight))

    def getLeft: Rep[A] = struct_getLeft(o)
    def getRight: Rep[B] = struct_getRight(o)
  }

  /**
   * operations handled by the Exp world
   */
  def left[A: Typ: Nul, B: Typ: Nul](a: Rep[A]): Rep[Either[A, B]]
  def right[A: Typ: Nul, B: Typ: Nul](b: Rep[B]): Rep[Either[A, B]]

  def struct_isLeft[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[Boolean]
  def struct_getLeft[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[A]
  def struct_getRight[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[B]

}

trait EitherOpsExp
    extends EitherOps
    with IfThenElseExp
    with BooleanOpsExp
    with StructExp
    //with CastingOpsExp
    with EqualExp
    /*with ZeroValExp*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def either_typ[A: Typ, B: Typ]: Typ[Either[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  implicit def either_nul[A: Typ: Nul, B: Typ: Nul] = new Nul[Either[A, B]] {
    def nullValue = unit(null.asInstanceOf[scala.Either[A, B]])
    /**
     * we are not interested in the underlying null instances here
     */
    def nlArguments = Nil
  }

  implicit def make_either[A: Typ: Nul, B: Typ: Nul](o: Either[Rep[A], Rep[B]])(implicit pos: SourceContext): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> o.left.getOrElse(zeroVal[A]),
      "right" -> o.right.getOrElse(zeroVal[B]),
      "isLeft" -> unit(o match { case Left(_) => true; case _ => false })
    )

  def left[A: Typ: Nul, B: Typ: Nul](a: Rep[A]): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> a,
      "right" -> zeroVal[B],
      "isLeft" -> unit(true)
    )

  def right[A: Typ: Nul, B: Typ: Nul](b: Rep[B]): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> zeroVal[A],
      "right" -> b,
      "isLeft" -> unit(false)
    )

  def struct_isLeft[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[Boolean] = field[Boolean](e, "isLeft")
  def struct_getLeft[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[A] = field[A](e, "left")
  def struct_getRight[A: Typ: Nul, B: Typ: Nul](e: Rep[Either[A, B]]): Rep[B] = field[B](e, "right")
}

trait EitherOpsExpOpt
    extends EitherOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with StructExpOpt
    with EqualExpOpt

trait EitherGenBase extends GenericCodegen with BaseGenStruct {
  val IR: EitherOpsExp
  import IR._

  override def remap[A](m: Typ[A]) = m.erasure.getSimpleName match {
    case "Either" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenEitherOps extends ScalaGenBase with EitherGenBase with ScalaGenStruct
  /*with ScalaGenCastingOps*/ with ScalaGenEqual with ScalaGenIfThenElse { val IR: EitherOpsExp }

trait CGenEitherOps extends CGenBase with EitherGenBase with CGenStruct //with CGenCastingOps
  with CGenEqual with CGenIfThenElse { val IR: EitherOpsExp }

