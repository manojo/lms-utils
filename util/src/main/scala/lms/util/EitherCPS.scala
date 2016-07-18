package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Either
 * To see whether CPS encoding + code gen generates good code.
 *
 * Extending BaseExp just to be able to extend `Def`.
 * This allows us to have a Rep[EitherCPS]
 *
 * see http://manojo.github.io/2015/03/20/cps-encoding-either/ for more
 * details
 */
 trait EitherCPS
     extends Base
     with IfThenElse
     with BooleanOps
     with LiftVariables
     /*with ZeroVal*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def eithercps_typ[A: Typ, B: Typ]: Typ[EitherCPS[A, B]]
  implicit def eithercps_nul[A: Typ: Nul, B: Typ: Nul]: Nul[EitherCPS[A, B]]

  /**
   * A CPS encoding of Either: Either is a construct that takes a value
   * of type A or B and eventually produces a value of type X
   * This implementation is not directly used here, but is wrapped inside
   * and `EitherWrapper`, so that it's accessible in the `Exp` world.
   */
  abstract class EitherCPS[A: Typ: Nul, B: Typ: Nul] { self =>

    def apply[X: Typ: Nul](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X]

    def map[C: Typ: Nul, D: Typ: Nul](
        lmap: Rep[A] => Rep[C],
        rmap: Rep[B] => Rep[D]) = new EitherCPS[C, D] {

      def apply[X: Typ: Nul](
        lf: Rep[C] => Rep[X],
        rf: Rep[D] => Rep[X]) = self.apply(a => lf(lmap(a)), b => rf(rmap(b)))
    }
  }

  /**
   * Companion object
   */
  object EitherCPS {

    def LeftCPS[A: Typ: Nul, B: Typ: Nul](a: Rep[A]) = new EitherCPS[A, B] {
      def apply[X: Typ: Nul](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        lf(a)
    }

    def RightCPS[A: Typ: Nul, B: Typ: Nul](b: Rep[B]) = new EitherCPS[A, B] {
      def apply[X: Typ: Nul](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]) =
        rf(b)
    }

    def conditional[A: Typ: Nul, B: Typ: Nul](
      cond: Rep[Boolean],
      thenp: => EitherCPS[A, B],
      elsep: => EitherCPS[A, B]
    ): EitherCPS[A, B] = {

      new EitherCPS[A, B] {
        def apply[X: Typ: Nul](
            lf: Rep[A] => Rep[X],
            rf: Rep[B] => Rep[X]): Rep[X] = {

          var tmpEither = zeroVal[EitherCPS[A, B]]

          val lCont = (a: Rep[A]) => { tmpEither = mkLeft[A, B](a) }
          val rCont = (b: Rep[B]) => { tmpEither = mkRight[A, B](b) }

          if (cond) thenp.apply[Unit](lCont, rCont)
          else      elsep.apply[Unit](lCont, rCont)

          val e = readVar(tmpEither)
          if (e.isLeft) lf(e.l) else rf(e.r)
        }
      }
    }
  }

  /**
   * Pimping my ride, so I can write DSL style code at the interface
   */
  implicit class EitherCPSCls[A: Typ: Nul, B: Typ: Nul](e: Rep[EitherCPS[A, B]]) {
    def map[C: Typ: Nul, D: Typ: Nul](
      lmap: Rep[A] => Rep[C],
      rmap: Rep[B] => Rep[D]
    ): Rep[EitherCPS[C, D]] = eitherCPS_map(e, lmap, rmap)

    def apply[X: Typ: Nul](lf: Rep[A] => Rep[X], rf: Rep[B] => Rep[X]): Rep[X] =
      either_apply(e, lf, rf)

    def isLeft: Rep[Boolean] = either_isLeft(e)
    def l: Rep[A] = either_l(e)
    def r: Rep[B] = either_r(e)
  }

  /**
   * interface-level functions
   */
  def mkLeft[A: Typ: Nul, B: Typ: Nul](a: Rep[A]): Rep[EitherCPS[A, B]]
  def mkRight[A: Typ: Nul, B: Typ: Nul](b: Rep[B]): Rep[EitherCPS[A, B]]

  def eitherCPS_map[A: Typ: Nul, B: Typ: Nul, C: Typ: Nul, D: Typ: Nul](
    e: Rep[EitherCPS[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherCPS[C, D]]

  def either_apply[A: Typ: Nul, B: Typ: Nul, X: Typ: Nul](
    e: Rep[EitherCPS[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X]

  def either_isLeft[A: Typ: Nul, B: Typ: Nul](e: Rep[EitherCPS[A, B]]): Rep[Boolean]
  def either_l[A: Typ: Nul, B: Typ: Nul](e: Rep[EitherCPS[A, B]]): Rep[A]
  def either_r[A: Typ: Nul, B: Typ: Nul](e: Rep[EitherCPS[A, B]]): Rep[B]

  def either_conditional[A: Typ: Nul, B: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  ): Rep[EitherCPS[A, B]]

  def __ifThenElse[A: Typ: Nul, B: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  ) = either_conditional(cond, thenp, elsep)
}

trait EitherCPSExp
    extends EitherCPS
    with BaseExp
    with IfThenElseExp
    with BooleanOpsExp
    with EqualExp
    //with ZeroValExp
    /** this trait should be mixed in higher up */ with PrimitiveOpsExp {

  import EitherCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def eithercps_typ[A: Typ, B: Typ]: Typ[EitherCPS[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  implicit def eithercps_nul[A: Typ: Nul, B: Typ: Nul] = new Nul[EitherCPS[A, B]] {

    /**
     * We create a "left-leaning" either by default. It does not really matter
     * since we will never read from a null either.
     */
    def nullValue = mkEither(zeroVal[A], zeroVal[B], unit(true))
    def nlArguments = nul[A] :: nul[B] :: Nil
  }

  /**
   * overriding variable assignment specifically for EitherCPS
   */
  case class EitherVar[A: Typ: Nul, B: Typ: Nul](
    a: Var[A],
    b: Var[B],
    isLeft: Var[Boolean]) extends Def[Variable[EitherCPS[A, B]]]

  def mkEitherVar[A: Typ: Nul, B: Typ: Nul](
    a: Var[A],
    b: Var[B],
    isLeft: Var[Boolean]): Exp[Variable[EitherCPS[A, B]]] = EitherVar(a, b, isLeft)

  /**
   * Inspired from the Structs variable treatment at https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/src/common/Structs.scala
   * With appropriate ``annotation hacks'' where needed.
   */
  override def var_new[T: Typ: Nul](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {
    case Const(EitherStruct(a, b, isLeft)) =>

      val new_a = var_new(a)(a.tp, a.nl, pos)
      val new_b = var_new(b)(b.tp, b.nl, pos)
      val new_isL = var_new(isLeft)

      /**
       * We know that we will have an Exp of type Var T, but the type system does not,
       * unfortunately, hence the cast.
       */
      val pairVar: Exp[Var[T]] = mkEitherVar(new_a, new_b, new_isL)(
        mtype(a.tp), ntype(a.nl),
        mtype(b.tp), ntype(b.nl)).asInstanceOf[Exp[Var[T]]]

      Variable(pairVar)

    case _ => super.var_new(init)
  }

  /**
   * Inspired from the Structs variable treatment at https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/src/common/Structs.scala
   * With appropriate ``annotation hacks'' where needed.
   */
  override def var_assign[T: Typ: Nul](lhs: Var[T], rhs: Exp[T])
                                      (implicit pos: SourceContext): Exp[Unit] = {
    (lhs, rhs) match {
      case (Variable(Def(Reflect(EitherVar(v1, v2, v3), _, _))),
            Const(EitherStruct(a, b, isLeft))) =>

        var_assign(v1, a)(a.tp, a.nl, pos)
        var_assign(v2, b)(b.tp, b.nl, pos)
        var_assign(v3, isLeft)

      case _ => super.var_assign(lhs, rhs)
    }
  }

  override def readVar[T: Typ: Nul](v: Var[T])(implicit pos: SourceContext): Exp[T] = v match {
    case Variable(Def(Reflect(EitherVar(v1 @ Variable(a), v2 @ Variable(b), v3), _, _))) =>

      /**
       * critical to pass the correct implicits to readVar
       * as well as to makeEither. `makeEither` only needs A, B in EitherCPS[A, B]
       * and so we must deconstruct as appropriate.
       *
       * Since we are in the EitherCPS[A] case there will always be
       * two element in typ[T]
       */

      val ta :: tb :: _ = typ[T].typeArguments
      val na :: nb :: _ = nul[T].nlArguments

      mkEither(
        readVar(v1)(
          mtype(a.tp.typeArguments.head),
          ntype(a.nl.nlArguments.head),
          pos
        ),
        readVar(v2)(
          mtype(b.tp.typeArguments.head),
          ntype(b.nl.nlArguments.head),
          pos
        ),
        readVar(v3)
      )(mtype(ta), ntype(na), mtype(tb), ntype(nb)).asInstanceOf[Exp[T]]

    case _ => super.readVar(v)
  }

  /**
   * An extra wrapper in the Exp world to make handling variables a bit easier
   */
  case class EitherStruct[A: Typ: Nul, B: Typ: Nul](
      a: Rep[A], b: Rep[B], isLeft: Rep[Boolean]) extends EitherCPS[A, B] {

    def apply[X: Typ: Nul](
        lf: Rep[A] => Rep[X],
        rf: Rep[B] => Rep[X]): Rep[X] = isLeft match {

      case Const(true) => lf(a)
      case Const(false) => rf(b)
      case _ => if (isLeft) lf(a) else rf(b)
    }
  }

  /**
   * dummy typeclasses to satisfy using `unit(EitherStruct(..))`
   */
  implicit def eitherstruct_typ[A: Typ, B: Typ]: Typ[EitherStruct[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  implicit def eitherstruct_nul[A: Typ: Nul, B: Typ: Nul] = new Nul[EitherStruct[A, B]] {
    def nullValue = null
    def nlArguments = Nil
  }

  def mkLeft[A: Typ: Nul, B: Typ: Nul](a: Rep[A]): Rep[EitherCPS[A, B]] =
    unit(EitherStruct(a, zeroVal[B], unit(true)))

  def mkRight[A: Typ: Nul, B: Typ: Nul](b: Rep[B]): Rep[EitherCPS[A, B]] =
    unit(EitherStruct(zeroVal[A], b, unit(false)))

  /** constructor for life simplification */
  def mkEither[A: Typ: Nul, B: Typ: Nul](a: Rep[A], b: Rep[B], isLeft: Rep[Boolean]) =
    unit(EitherStruct(a, b, isLeft))

  /**
   * Both the functions below will misbehave if we have some other representation
   * of `EitherCPS`. Which may be uncool at codegen time. But then again,
   * if that happens, we are probably doing something wrong-ish, so it's kind
   * of a sanity check
   */
  def eitherCPS_map[A: Typ: Nul, B: Typ: Nul, C: Typ: Nul, D: Typ: Nul](
    e: Rep[EitherCPS[A, B]],
    lmap: Rep[A] => Rep[C],
    rmap: Rep[B] => Rep[D]
  ): Rep[EitherCPS[C, D]] = e match {
    case Const(sth) => unit(sth map (lmap, rmap))
  }

  def either_apply[A: Typ: Nul, B: Typ: Nul, X: Typ: Nul](
    e: Rep[EitherCPS[A, B]],
    lf: Rep[A] => Rep[X],
    rf: Rep[B] => Rep[X]
  ): Rep[X] = e match {
    case Const(sth) => sth.apply(lf, rf)
  }

  /**
   * a 'conditional' either
   * lifts conditional expressions to Either level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def either_conditional[A: Typ: Nul, B: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[EitherCPS[A, B]],
    elsep: => Rep[EitherCPS[A, B]]
  ): Rep[EitherCPS[A, B]] = (thenp, elsep) match { //stricting them here
    case (Const(t), Const(e)) => unit(conditional(cond, t, e))
  }

  def either_isLeft[A: Typ: Nul, B: Typ: Nul](
      e: Rep[EitherCPS[A, B]]): Rep[Boolean] = e match {
    case Const(EitherStruct(_, _, isL)) => isL
  }

  def either_l[A: Typ: Nul, B: Typ: Nul](
      e: Rep[EitherCPS[A, B]]): Rep[A] = e match {
    case Const(EitherStruct(a, _, _)) => a
  }

  def either_r[A: Typ: Nul, B: Typ: Nul](
      e: Rep[EitherCPS[A, B]]): Rep[B] = e match {
    case Const(EitherStruct(_, b, _)) => b
  }
}

/**
 * Specific code generator for EitherCPS that doesn't generate EitherVars
 */
trait EitherCPSGenBase extends GenericCodegen {
  val IR: EitherCPSExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EitherVar(_, _, _) => ()
    case _ => super.emitNode(sym, rhs)
  }
}
