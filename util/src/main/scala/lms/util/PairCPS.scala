package lms.util

import lms._

import scala.lms.common._
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * A CPS encoding of Pairs
 * an alternative to the struct representation
 */
trait PairCPS
    extends Base
    with IfThenElse
    with BooleanOps
    with LiftVariables
    with TupleOps
    /*with ZeroVal*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def paircps_typ[A: Typ, B: Typ]: Typ[PairCPS[A, B]]
  implicit def paircps_nul[A: Typ: Nul, B: Typ: Nul]: Nul[PairCPS[A, B]]

  /**
   * CPS encoding for Option
   * isDefined does not make sense for this encoding
   */
  abstract class PairCPS[A: Typ: Nul, B: Typ: Nul] { self =>

    def apply[X: Typ: Nul](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X]

    def map[C: Typ: Nul, D: Typ: Nul](
        f: Rep[A] => Rep[C],
        g: Rep[B] => Rep[D]) = new PairCPS[C, D] {

      def apply[X: Typ: Nul](k: (Rep[C], Rep[D]) => Rep[X]) =
        self.apply((a, b) => k(f(a), g(b)))
    }

    def toPair: Rep[(A, B)] = self.apply((a, b) => make_tuple2(a, b))
  }

  /**
   * Companion object
   */
  object PairCPS {

    def Pair[A: Typ: Nul, B: Typ: Nul](a: Rep[A], b: Rep[B]) = new PairCPS[A, B] {
      def apply[X: Typ: Nul](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = k(a, b)
    }

    /**
     * a conditional expression for PairCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     */
    def conditional[A: Typ: Nul, B: Typ: Nul](
      cond: Rep[Boolean],
      thenp: => PairCPS[A, B],
      elsep: => PairCPS[A, B]
    ): PairCPS[A, B] = new PairCPS[A, B] {

      def apply[X: Typ: Nul](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = {

        var tmpPair = zeroVal[PairCPS[A, B]]

        val assignK = (a: Rep[A], b: Rep[B]) => { tmpPair = mkPair(a, b) }

        if (cond) thenp.apply(assignK) else elsep.apply(assignK)
        k(readVar(tmpPair)._1, readVar(tmpPair)._2)
      }
    }
  }

  /**
   * Pimping my ride, now I have access to Rep[PairCPS]
   */
  implicit class PairCPSCls[A: Typ: Nul, B: Typ: Nul](pair: Rep[PairCPS[A, B]]) {

    def map[C: Typ: Nul, D: Typ: Nul](f: Rep[A] => Rep[C], g: Rep[B] => Rep[D]) =
      paircps_map(pair, f, g)

    def apply[X: Typ: Nul](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] =
      paircps_apply(pair, k)

    def toPair: Rep[(A, B)] = paircps_toPair(pair)

    def _1: Rep[A] = paircps_1(pair)
    def _2: Rep[B] = paircps_2(pair)

    /**
     * for now we don't include other operations on pairCPS, we don't
     * seem to need them
     */
  }

  /**
   * interface level functions
   */
  def mkPair[A: Typ: Nul, B: Typ: Nul](a: Rep[A], b: Rep[B]): Rep[PairCPS[A, B]]

  def paircps_map[A: Typ: Nul, B: Typ: Nul, C: Typ: Nul, D: Typ: Nul](
    pair: Rep[PairCPS[A, B]],
    f: Rep[A] => Rep[C],
    g: Rep[B] => Rep[D]): Rep[PairCPS[C, D]]

  def paircps_apply[A: Typ: Nul, B: Typ: Nul, X: Typ: Nul](
    pair: Rep[PairCPS[A, B]],
    k: (Rep[A], Rep[B]) => Rep[X]): Rep[X]

  def pair_conditional[A: Typ: Nul, B: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[PairCPS[A, B]],
    elsep: => Rep[PairCPS[A, B]]): Rep[PairCPS[A, B]]

  //def __ifThenElse[A: Typ: Nul, B: Typ: Nul](
  //  cond: Rep[Boolean],
  //  thenp: => Rep[PairCPS[A, B]],
  //  elsep: => Rep[PairCPS[A, B]]) = pair_conditional(cond, thenp, elsep)

  def paircps_toPair[A: Typ: Nul, B: Typ: Nul](pair: Rep[PairCPS[A, B]]): Rep[(A, B)]
  def paircps_1[A: Typ: Nul, B: Typ: Nul](pair: Rep[PairCPS[A, B]]): Rep[A]
  def paircps_2[A: Typ: Nul, B: Typ: Nul](pair: Rep[PairCPS[A, B]]): Rep[B]
}

trait PairCPSExp
    extends PairCPS
    with BaseExp
    with TupleOpsExp
    with IfThenElseExp
    with BooleanOpsExp
    with OptionOpsExp
    /*with ZeroValExp*/ {

  import PairCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def paircps_typ[A: Typ, B: Typ]: Typ[PairCPS[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  implicit def paircps_nul[A: Typ: Nul, B: Typ: Nul] = new Nul[PairCPS[A, B]] {
    def nullValue = mkPair(zeroVal[A], zeroVal[B])
    def nlArguments = nul[A] :: nul[B] :: Nil
  }

  /**
   * overriding variable assignment specifically for PairCPS
   */
  case class PairVar[A: Typ: Nul, B: Typ: Nul](
    a: Var[A],
    b: Var[B]) extends Def[Variable[PairCPS[A, B]]]

  def mkPairVar[A: Typ: Nul, B: Typ: Nul](
    a: Var[A],
    b: Var[B]): Exp[Variable[PairCPS[A, B]]] = PairVar(a, b)

  /**
   * Inspired from the Structs variable treatment at https://github.com/TiarkRompf/virtualization-lms-core/blob/develop/src/common/Structs.scala
   * With appropriate ``annotation hacks'' where needed.
   */
  override def var_new[T: Typ: Nul](init: Exp[T])(implicit pos: SourceContext): Var[T] = init match {
    case Def(PairWrapper(PairStruct(a, b))) =>

      val new_a = var_new(a)(a.tp, a.nl, pos)
      val new_b = var_new(b)(b.tp, b.nl, pos)

      /**
       * We know that we will have an Exp of type Var T, but the type system does not,
       * unfortunately, hence the cast.
       */
      val pairVar: Exp[Var[T]] = mkPairVar(new_a, new_b)(
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
      case (Variable(Def(Reflect(PairVar(v1, v2), _, _))),
            Def(PairWrapper(p))) =>

        p apply { (a, b) =>
          var_assign(v1, a)(a.tp, a.nl, pos)
          var_assign(v2, b)(b.tp, b.nl, pos)
        }

      case _ => super.var_assign(lhs, rhs)
    }
  }

  override def readVar[T: Typ: Nul](v: Var[T])(implicit pos: SourceContext): Exp[T] = v match {
    case Variable(Def(Reflect(PairVar(v1 @ Variable(a), v2 @ Variable(b)), _, _))) =>

      /**
       * critical to pass the correct implicits to readVar
       * as well as to makePair. `makePair` only needs A, B in PairCPS[A]
       * and so we must deconstruct as appropriate.
       *
       * Since we are in the PairCPS[A, B] case there will always be
       * two elements in typ[T]
       */

      val ta :: tb :: _ = typ[T].typeArguments
      val na :: nb :: _ = nul[T].nlArguments

      mkPair(
        readVar(v1)(
          mtype(a.tp.typeArguments.head),
          ntype(a.nl.nlArguments.head),
          pos
        ),
        readVar(v2)(
          mtype(b.tp.typeArguments.head),
          ntype(b.nl.nlArguments.head),
          pos
        )
      )(mtype(ta), ntype(na), mtype(tb), ntype(nb)).asInstanceOf[Exp[T]]

    case _ => super.readVar(v)
  }



  /**
   * The wrapper acting as Rep[PairCPS[A]]
   */
  case class PairWrapper[A: Typ: Nul, B: Typ: Nul](p: PairCPS[A, B])
    extends Def[PairCPS[A, B]]

  /**
   * An extra wrapper in the Exp world to make handling variables a bit easier
   */
  case class PairStruct[A: Typ: Nul, B: Typ: Nul](
      a: Rep[A], b: Rep[B]) extends PairCPS[A, B] {

    def apply[X: Typ: Nul](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = k(a, b)
  }

  /**
   * dummy typeclasses to satisfy using `unit(PairStruct(..))`
   */
  implicit def pairstruct_typ[A: Typ, B: Typ]: Typ[PairStruct[A, B]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mB) = typ[B]
    manifestTyp
  }

  implicit def pairstruct_nul[A: Typ: Nul, B: Typ: Nul] = new Nul[PairStruct[A, B]] {
    def nullValue = null
    def nlArguments = Nil
  }

  def mkPair[A: Typ: Nul, B: Typ: Nul](a: Rep[A], b: Rep[B]): Exp[PairCPS[A, B]]
    = PairWrapper(PairStruct(a, b))

  def paircps_map[A: Typ: Nul, B: Typ: Nul, C: Typ: Nul, D: Typ: Nul](
      pair: Rep[PairCPS[A, B]],
      f: Rep[A] => Rep[C],
      g: Rep[B] => Rep[D]): Rep[PairCPS[C, D]] = pair match {
    case Def(PairWrapper(pair)) => PairWrapper(pair.map(f, g))
  }

  def paircps_apply[A: Typ: Nul, B: Typ: Nul, X: Typ: Nul](
      pair: Rep[PairCPS[A, B]],
      k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = pair match {
    case Def(PairWrapper(pair)) => pair(k)
  }

  /**
   * a 'conditional' pair
   * lifts conditional expressions to pair level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def pair_conditional[A: Typ: Nul, B: Typ: Nul](
    cond: Rep[Boolean],
    thenp: => Rep[PairCPS[A, B]],
    elsep: => Rep[PairCPS[A, B]]
  ): Rep[PairCPS[A, B]] = (thenp, elsep) match { //stricting them here
    case (Const(t), Const(e)) =>
      PairWrapper(conditional(cond, t, e))
  }

  def paircps_toPair[A: Typ: Nul, B: Typ: Nul](
      pair: Rep[PairCPS[A, B]]): Rep[(A, B)] = pair match {
    case Def(PairWrapper(pair)) => pair.toPair
  }

  def paircps_1[A: Typ: Nul, B: Typ: Nul](
      pair: Rep[PairCPS[A, B]]): Rep[A] = pair match {
    case Def(PairWrapper(PairStruct(a, _))) => a
    case Def(PairWrapper(p)) => p.apply((a, _) => a)
  }

  def paircps_2[A: Typ: Nul, B: Typ: Nul](
      pair: Rep[PairCPS[A, B]]): Rep[B] = pair match {
    case Def(PairWrapper(PairStruct(_, b))) => b
    case Def(PairWrapper(p)) => p.apply((_, b) => b)
  }

}

/**
 * Specific code generator for PairCPS that doesn't generate PairVars
 */
trait PairCPSGenBase extends GenericCodegen {
  val IR: PairCPSExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PairVar(_, _) => ()
    case _ => super.emitNode(sym, rhs)
  }
}
