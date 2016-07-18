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
    /*with ZeroVal*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def optioncps_typ[A: Typ]: Typ[OptionCPS[A]]
  implicit def optioncps_nul[A: Typ: Nul]: Nul[OptionCPS[A]]

  /**
   * CPS encoding for Option
   * isDefined does not make sense for this encoding
   */
  abstract class OptionCPS[T: Typ: Nul] { self =>

    def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X]

    def map[U: Typ: Nul](f: Rep[T] => Rep[U]) = new OptionCPS[U] {
      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) = {
        self.apply(none, (t: Rep[T]) => some(f(t)))
      }
    }

    def flatMap[U: Typ: Nul](f: Rep[T] => OptionCPS[U]) = new OptionCPS[U] {
      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[U] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => f(t).apply(none, some))
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new OptionCPS[T] {
      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]) =
        self.apply(none, (t: Rep[T]) => if (p(t)) some(t) else none(()))
    }

    /**
     * helper method that introduces vars and eventually yields a Rep[Option]
     */
    def toOption: Rep[Option[T]] = {
      self.apply(
        (_: Rep[Unit]) => none[T](),
        x => make_opt(scala.Some(x))
      )
    }
  }

  /**
   * Companion object
   */
  object OptionCPS {
    def Some[T: Typ: Nul](t: Rep[T]) = new OptionCPS[T] {
      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        some(t)
    }

    def None[T: Typ: Nul] = new OptionCPS[T] {
      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] =
        none(())
    }

    /**
     * a conditional expression for OptionCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     */
    def conditional[T: Typ: Nul](
      cond: Rep[Boolean],
      thenp: => OptionCPS[T],
      elsep: => OptionCPS[T]
    ): OptionCPS[T] = new OptionCPS[T] {

      def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[T] => Rep[X]): Rep[X] = {

        var tmpOpt = zeroVal[OptionCPS[T]]

        val noneK = (_: Rep[Unit]) => unit(())
        val someK = (t: Rep[T]) => { tmpOpt = mkSome(t) }

        if (cond) thenp.apply(noneK, someK) else elsep.apply(noneK, someK)
        if (readVar(tmpOpt).isDefined) some(readVar(tmpOpt).value) else none(unit(()))
      }
    }
  }

  /**
   * Pimping my ride, now I have access to Rep[OptionCPS]
   */
  implicit class OptionCPSCls[A: Typ: Nul](opt: Rep[OptionCPS[A]]) {
    def map[B: Typ: Nul](f: Rep[A] => Rep[B]): Rep[OptionCPS[B]] = optioncps_map(opt, f)

    def apply[X: Typ: Nul](none: Rep[Unit] => Rep[X], some: Rep[A] => Rep[X]): Rep[X] =
      optioncps_apply(opt, none, some)

    def flatMap[B: Typ: Nul](f: Rep[A] => OptionCPS[B]): Rep[OptionCPS[B]] =
      optioncps_flatmap(opt, f)

    def filter(p: Rep[A] => Rep[Boolean]): Rep[OptionCPS[A]] =
      optioncps_filter(opt, p)

    def toOption: Rep[Option[A]] = optioncps_toOption(opt)

    /**
     * These operations only exists on Rep[OptionCPS[A]] since in a proper CPS encoding
     * we would not need it
     */
    def value: Rep[A] = optioncps_value(opt)
    def isDefined: Rep[Boolean] = optioncps_isDefined(opt)

    /**
     * for now we don't include other operations on optionCPS, we don't
     * seem to need them
     */
  }

  /**
   * interface level functions
   */
  def mkSome[A: Typ: Nul](a: Rep[A]): Rep[OptionCPS[A]]
  def mkNone[A: Typ: Nul]: Rep[OptionCPS[A]]

  def optioncps_map[A: Typ: Nul, B: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => Rep[B]
  ): Rep[OptionCPS[B]]

  def optioncps_apply[A: Typ: Nul, X: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    none: Rep[Unit] => Rep[X],
    some: Rep[A] => Rep[X]
  ): Rep[X]

  def optioncps_flatmap[A: Typ: Nul, B: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => OptionCPS[B]
  ): Rep[OptionCPS[B]]

  def optioncps_filter[A: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    p: Rep[A] => Rep[Boolean]
  ): Rep[OptionCPS[A]]

  def option_conditional[A: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ): Rep[OptionCPS[A]]

  def __ifThenElse[A: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ) = option_conditional(cond, thenp, elsep)

  def optioncps_toOption[A: Typ: Nul](
    opt: Rep[OptionCPS[A]]
  ): Rep[Option[A]]

  def optioncps_value[A: Typ: Nul](opt: Rep[OptionCPS[A]]): Rep[A]
  def optioncps_isDefined[A: Typ: Nul](opt: Rep[OptionCPS[A]]): Rep[Boolean]

}

trait OptionCPSExp
    extends OptionCPS
    with BaseExp
    with IfThenElseExp
    with BooleanOpsExp
    with OptionOpsExp
    with VariablesExp
    /*with ZeroValExp*/ {

  import OptionCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def optioncps_typ[A: Typ]: Typ[OptionCPS[A]] = {
    implicit val ManifestTyp(mA) = typ[A]
    manifestTyp
  }

  implicit def optioncps_nul[A: Typ: Nul] = new Nul[OptionCPS[A]] {
    def nullValue = mkNone[A]
    def nlArguments = nul[A] :: Nil
  }

  /**
   * overriding variable assignment specifically for OptionCPS
   */
  case class OptionVar[A: Typ: Nul](isDefined: Var[Boolean], value: Var[A])
    extends Def[Variable[OptionCPS[A]]]

  def mkOptionVar[A: Typ: Nul](
    isDefined: Var[Boolean],
    value: Var[A]): Exp[Variable[OptionCPS[A]]] = OptionVar(isDefined, value)

  /**
   * Inspired from the Structs variable treatment at https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/src/common/Structs.scala
   * With appropriate ``annotation hacks'' where needed.
   */
  override def var_new[T: Typ: Nul](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {

    /**
     * Assuming initalizing in only ever done by mkSome, mkNone
     */
    case Def(OptionWrapper(OptStruct(idf, t))) =>

      val new_idf = var_new(idf)
      val new_t = var_new(t)(t.tp, t.nl, pos)

      /**
       * We know that we will have an Exp of type Var T, but the type system does not,
       * unfortunately, hence the cast.
       *
       * Note how we pass `t.tp` and `t.nl` to `mkOptionVar`, instead of  the tp of
       * the `init.tp`.
       * This is because `init.tp` has an extra `OptionCPS` wrapped around it.
       */
      val optVar: Exp[Var[T]] =
        mkOptionVar(new_idf, new_t)(mtype(t.tp), ntype(t.nl)).asInstanceOf[Exp[Var[T]]]

      Variable(optVar)

    case _ => super.var_new(init)
  }

  /**
   * Inspired from the Structs variable treatment at https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/src/common/Structs.scala
   * With appropriate ``annotation hacks'' where needed.
   */
  override def var_assign[T: Typ: Nul](lhs: Var[T], rhs: Exp[T])
                                      (implicit pos: SourceContext): Exp[Unit] = {
    (lhs, rhs) match {
      case (Variable(Def(Reflect(OptionVar(isDefined, value), _, _))),
            Def(OptionWrapper(opt))) => opt match {

        case OptStruct(idf, t) =>
          var_assign(isDefined, idf)
          var_assign(value, t)(t.tp, t.nl, pos)

        case _ => opt.apply(
          _ => var_assign(isDefined, unit(false)), // we assign nothing to the value; var_assign(value, zeroVal[T])(t.tp, t.nl, pos)
          t => { var_assign(isDefined, unit(true)); var_assign(value, t)(t.tp, t.nl, pos) }
        )
      }

      case _ => super.var_assign(lhs, rhs)
    }
  }

  override def readVar[T: Typ: Nul](v: Var[T])
                                   (implicit pos: SourceContext): Exp[T] = v match {

    case Variable(Def(Reflect(OptionVar(isDefined, v2 @ Variable(value)), _, _))) =>

      /**
       * critical to pass the correct implicits to readVar
       * as well as to makeOpt. `makeOpt` only needs the A in OptionCPS[A]
       * and so we must deconstruct as appropriate.
       *
       * Since we are in the OptionCPS[A] case there will always be
       * a head element to typ[T]
       */
      mkOpt(
        readVar(isDefined),
        readVar(v2)(
          mtype(value.tp.typeArguments.head),
          ntype(value.nl.nlArguments.head),
          pos
        )
      )(mtype(typ[T].typeArguments.head), ntype(nul[T].nlArguments.head)).asInstanceOf[Exp[T]]

    case _ => super.readVar(v)
  }

  /**
   * The wrapper acting as Rep[OptionCPS[A]]
   */
  case class OptionWrapper[A: Typ: Nul](e: OptionCPS[A]) extends Def[OptionCPS[A]]

  /**
   * An extra wrapper in the Exp world to make handling variables a bit easier
   */
  case class OptStruct[T: Typ: Nul](
      isDefined: Rep[Boolean],
      t: Rep[T]) extends OptionCPS[T] {

    def apply[X: Typ: Nul](
        none: Rep[Unit] => Rep[X],
        some: Rep[T] => Rep[X]
    ): Rep[X] = isDefined match {
      case Const(true) => some(t)
      case Const(false) => none(())
      case _ => if (isDefined) some(t) else none(())
    }
  }

  /**
   * dummy typeclasses to satisfy using `unit(OptStruct(..))`
   */
  implicit def optstruct_typ[A: Typ]: Typ[OptStruct[A]] = {
    implicit val ManifestTyp(mA) = typ[A]
    manifestTyp
  }

  implicit def optstruct_nul[A: Typ: Nul] = new Nul[OptStruct[A]] {
    def nullValue = null
    def nlArguments = Nil
  }

  def mkSome[A: Typ: Nul](a: Rep[A]): Rep[OptionCPS[A]] = mkOpt(unit(true), a)
  def mkNone[A: Typ: Nul]: Rep[OptionCPS[A]] = mkOpt(unit(false), zeroVal[A])

  /** constructor for life simplification */
  def mkOpt[A: Typ: Nul](isDefined: Rep[Boolean], a: Rep[A]): Exp[OptionCPS[A]] =
    OptionWrapper(OptStruct(isDefined, a))

  def optioncps_map[A: Typ: Nul, B: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => Rep[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt map f)
  }

  def optioncps_flatmap[A: Typ: Nul, B: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    f: Rep[A] => OptionCPS[B]
  ): Rep[OptionCPS[B]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt flatMap f)
  }

  def optioncps_filter[A: Typ: Nul](
    opt: Rep[OptionCPS[A]],
    p: Rep[A] => Rep[Boolean]
  ): Rep[OptionCPS[A]] = opt match {
    case Def(OptionWrapper(opt)) => OptionWrapper(opt filter p)
  }

  def optioncps_apply[A: Typ: Nul, X: Typ: Nul](
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
  def option_conditional[A: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[OptionCPS[A]],
    elsep: => Rep[OptionCPS[A]]
  ): Rep[OptionCPS[A]] = (thenp, elsep) match {
    case (Def(OptionWrapper(t)), Def(OptionWrapper(e))) =>
      OptionWrapper(conditional(cond, t, e))
  }

  def optioncps_toOption[A: Typ: Nul](
    opt: Rep[OptionCPS[A]]
  ): Rep[Option[A]] = opt match {
    case Def(OptionWrapper(opt)) => opt.toOption
  }

  def optioncps_value[A: Typ: Nul](opt: Rep[OptionCPS[A]]): Rep[A] = opt match {
    case Def(OptionWrapper(OptStruct(_, v))) => v
    case Def(OptionWrapper(opt)) => opt.apply(_ => zeroVal[A], t => t)
  }

  def optioncps_isDefined[A: Typ: Nul](opt: Rep[OptionCPS[A]]): Rep[Boolean] = opt match {
    case Def(OptionWrapper(OptStruct(idf, _))) => idf
    case Def(OptionWrapper(opt)) => opt.apply(_ => unit(false), t => unit(false))
  }
}

/**
 * Specific code generator for OptionCPS that doesn't generate OptionVars
 */
trait OptionCPSGenBase extends GenericCodegen {
  val IR: OptionCPSExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OptionVar(_, _) => ()
    case _ => super.emitNode(sym, rhs)
  }
}
