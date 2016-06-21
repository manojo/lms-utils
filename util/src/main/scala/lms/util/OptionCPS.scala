package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Option
 * an alternative to the struct representation
 */
trait OptionCPS
    extends Base
    with IfThenElse
    with BooleanOps
    with LiftVariables
    with OptionOps
    with ZeroVal {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def optioncps_typ[A: Typ]: Typ[OptionCPS[A]]

  /**
   * CPS encoding for Option
   * isDefined does not make sense for this encoding
   */
  abstract class OptionCPS[T: Typ] { self =>

    def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X]

    def map[U: Typ](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => some(f(t)))
    }

    def flatMap[U: Typ](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => f(t).apply(none, some))
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => if (p(t)) some(t) else none(()))
    }

    /**
     * helper method that introduces vars and eventually yields a Rep[Option]
     */
    def toOption: Rep[Option[T]] = {
      var isDefined = unit(false); var value = zeroVal[T]
      self.apply(
        (_: Rep[Unit]) => unit(()),
        x => { isDefined = unit(true); value = x }
      )
      if (isDefined) make_opt(scala.Some(readVar(value))) else none[T]()
    }

  }

  /**
   * Companion object
   */
  object OptionCPS {
    def Some[T: Typ](t: Rep[T]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        some(t)
    }

    def None[T: Typ] = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        none(())
    }

    /**
     * a conditional expression for OptionCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     *
     * This implementation becomes a join point. We could do more analysis
     * to get the full depth of nested conditionals, but is is safer
     * to do it this way. Having a specialised representation for `EitherCPSCond`
     * won't by itself solve the issues.
     */
    def conditional[T: Typ](
      cond: Rep[Boolean],
      thenp: => OptionCPS[T],
      elsep: => OptionCPS[T]
    ): OptionCPS[T] = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        if (cond) {
          thenp.apply(
            (_: Rep[Unit]) => unit(()),
            x => { isDefined = unit(true); value = x }
          )
        } else {
          elsep.apply(
            (_: Rep[Unit]) => unit(()),
            x => { isDefined = unit(true); value = x }
          )
        }

        if (isDefined) some(value) else none(unit(()))
      }
    }
  }

  /**
   * Pimping my ride, now I have access to Rep[OptionCPS]
   */
  implicit class OptionCPSCls[A: Typ](opt: Rep[OptionCPS[A]]) {
    def map[B: Typ](f: Rep[A] => Rep[B]): Rep[OptionCPS[B]] = optioncps_map(opt, f)

    def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[A] => Rep[X]): Rep[X] =
      optioncps_apply(opt, none, some)

    def flatMap[B: Typ](f: Rep[A] => OptionCPS[B]): Rep[OptionCPS[B]] =
      optioncps_flatmap(opt, f)

    def filter(p: Rep[A] => Rep[Boolean]): Rep[OptionCPS[A]] =
      optioncps_filter(opt, p)

    def toOption: Rep[Option[A]] = optioncps_toOption(opt)

    /**
     * for now we don't include other operations on optionCPS, we don't
     * seem to need them
     */
  }

  /**
   * interface level functions
   */
  def mkSome[A: Typ](a: Rep[A]): Rep[OptionCPS[A]]
  def mkNone[A: Typ]: Rep[OptionCPS[A]]

  def optioncps_map[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => Rep[B]
  ): Rep[OptionCPS[B]]

  def optioncps_apply[A: Typ, X: Typ](
    opt: Rep[OptionCPS[A]],
    none: Rep[Unit] => Rep[X],
    some: Rep[A] => Rep[X]
  ): Rep[X]

  def optioncps_flatmap[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => OptionCPS[B]
  ): Rep[OptionCPS[B]]

  def optioncps_filter[A: Typ](
    opt: Rep[OptionCPS[A]],
    p: Rep[A] => Rep[Boolean]
  ): Rep[OptionCPS[A]]

  def option_conditional[A: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ): Rep[OptionCPS[A]]

  def __ifThenElse[A: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ) = option_conditional(cond, thenp, elsep)

  def optioncps_toOption[A: Typ](
    opt: Rep[OptionCPS[A]]
  ): Rep[Option[A]]
}

trait OptionCPSExp
    extends OptionCPS
    with BaseExp
    with IfThenElseExp
    with BooleanOpsExp
    with OptionOpsExp
    with ZeroValExp {

  import OptionCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def optioncps_typ[A: Typ]: Typ[OptionCPS[A]] = {
    implicit val ManifestTyp(mA) = typ[A]
    manifestTyp
  }

  def mkSome[A: Typ](a: Rep[A]): Rep[OptionCPS[A]] = unit(Some(a))
  def mkNone[A: Typ]: Rep[OptionCPS[A]] = unit(None[A])

  def optioncps_map[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => Rep[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Const(opt) => unit(opt map f)
  }

  def optioncps_flatmap[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => OptionCPS[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Const(opt) => Const(opt flatMap f)
  }

  def optioncps_filter[A: Typ](
    opt: Rep[OptionCPS[A]],
    p: Rep[A] => Rep[Boolean]
  ): Rep[OptionCPS[A]] = opt match {
    case Const(opt) => Const(opt filter p)
  }

  def optioncps_apply[A: Typ, X: Typ](
    opt: Rep[OptionCPS[A]],
    none: Rep[Unit] => Rep[X],
    some: Rep[A] => Rep[X]
  ): Rep[X] = opt match {
    case Const(opt) => opt(none, some)
  }

  /**
   * a 'conditional' option
   * lifts conditional expressions to Option level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def option_conditional[A: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ): Rep[OptionCPS[A]] = (thenp, elsep) match { //stricting them here
    case (Const(t), Const(e)) => unit(conditional(cond, t, e))
  }

  def optioncps_toOption[A: Typ](
    opt: Rep[OptionCPS[A]]
  ): Rep[Option[A]] = opt match {
    case Const(opt) => opt.toOption
  }
}
