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
   * A node acting as a join point for OptionCPS
   */
  case class OptionCPSCond[T: Typ](
    cond: Rep[Boolean],
    t: OptionCPS[T],
    e: OptionCPS[T]
  ) extends OptionCPS[T] { self =>

    /**
     * naive apply function
     */
    def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
      if (cond) t(none, some) else e(none, some)

    /**
     * overriding implementations for the usual suspects
     * for a conditional, we don't want to inline higher order functions
     * in each branch.
     * For options, this is handy especially if both sides of the conditional yield
     * a Some. Otherwise it does not really matter, because no computation is performed
     * in the None case anyway. While codegen may be suboptimal for the latter case,
     * it's a tradeoff worth taking.
     *
     * Or is it? Maybe it's a better idea to impose only monadic style
     * composition for Option, and special case the append/orElse function for
     * join points?
     * Well, here's a counterexample:
     * Some(x) flatMap { x => (if (cond(x)) Some(y) else Some(z)).bigcomputation }
     * with such code, we still need the conditional notation. I guess tradeoff is
     * where things stand at the moment.
     * Unless we want to analyse the body of each conditional expression, etc. etc.
     * Also, at the moment, I (manojo) don't know the performance implications for
     * either code style.
     */

    override def map[U: Typ](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined) some(f(value)) else none(unit(()))
      }
    }

    override def flatMap[U: Typ](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined) f(value).apply(none, some) else none(unit(()))
      }
    }

    override def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Typ](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) = {
        var isDefined = unit(false); var value = zeroVal[T]

        self.apply(
          (_: Rep[Unit]) => unit(()),
          x => { isDefined = unit(true); value = x }
        )
        if (isDefined && p(value)) some(value) else none(unit(()))
      }
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
     */
    def conditional[T: Typ](
      cond: Rep[Boolean],
      thenp: => OptionCPS[T],
      elsep: => OptionCPS[T]
    ): OptionCPS[T] = OptionCPSCond(cond, thenp, elsep)
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

  /**
   * The wrapper acting as Rep[OptionCPS[A]]
   */
  case class OptionWrapper[A: Typ](e: OptionCPS[A]) extends Def[OptionCPS[A]]

  def mkSome[A: Typ](a: Rep[A]): Rep[OptionCPS[A]] = OptionWrapper(Some(a))
  def mkNone[A: Typ]: Rep[OptionCPS[A]] = OptionWrapper(None[A])

  def optioncps_map[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => Rep[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt map f)
  }

  def optioncps_flatmap[A: Typ, B: Typ](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => OptionCPS[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt flatMap f)
  }

  def optioncps_filter[A: Typ](
    opt: Rep[OptionCPS[A]],
    p: Rep[A] => Rep[Boolean]
  ): Rep[OptionCPS[A]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt filter p)
  }

  def optioncps_apply[A: Typ, X: Typ](
    opt: Rep[OptionCPS[A]],
    none: Rep[Unit] => Rep[X],
    some: Rep[A] => Rep[X]
  ): Rep[X] = opt match {
    case Def(OptionWrapper(opt)) => opt(none, some)
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
    case (Def(OptionWrapper(t)), Def(OptionWrapper(e))) =>
      OptionWrapper(conditional(cond, t, e))
  }

  def optioncps_toOption[A: Typ](
    opt: Rep[OptionCPS[A]]
  ): Rep[Option[A]] = opt match {
    case Def(OptionWrapper(opt)) => opt.toOption
  }
}
