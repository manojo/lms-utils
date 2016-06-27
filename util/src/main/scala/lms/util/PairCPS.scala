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
    with ZeroVal {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def paircps_typ[A: Typ, B: Typ]: Typ[PairCPS[A, B]]

  /**
   * CPS encoding for Option
   * isDefined does not make sense for this encoding
   */
  abstract class PairCPS[A: Typ, B: Typ] { self =>

    def apply[X: Typ](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X]

    def map[C: Typ, D: Typ](
        f: Rep[A] => Rep[C],
        g: Rep[B] => Rep[D]) = new PairCPS[C, D] {

      def apply[X: Typ](k: (Rep[C], Rep[D]) => Rep[X]) =
        self.apply((a, b) => k(f(a), g(b)))
    }

    def toPair: Rep[(A, B)] = self.apply((a, b) => make_tuple2(a, b))
  }

  /**
   * Companion object
   */
  object PairCPS {

    def mkPair[A: Typ, B: Typ](a: Rep[A], b: Rep[B]) = new PairCPS[A, B] {
      def apply[X: Typ](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = k(a, b)
    }

    /**
     * a conditional expression for PairCPS, mixed-stage
     * needs a different name than __ifThenElse because the latter requires
     * Rep `then` and `else` parameters
     */
    def conditional[A: Typ, B: Typ](
      cond: Rep[Boolean],
      thenp: => PairCPS[A, B],
      elsep: => PairCPS[A, B]
    ): PairCPS[A, B] = new PairCPS[A, B] {

      def apply[X: Typ](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = {

        var tmpA = zeroVal[A]; var tmpB = zeroVal[B]

        val assignK = (a: Rep[A], b: Rep[B]) => { tmpA = a; tmpB = b }

        if (cond) thenp.apply(assignK) else elsep.apply(assignK)
        k(readVar(tmpA), readVar(tmpB))
      }
    }
  }

  /**
   * Pimping my ride, now I have access to Rep[PairCPS]
   */
  implicit class PairCPSCls[A: Typ, B: Typ](pair: Rep[PairCPS[A, B]]) {

    def map[C: Typ, D: Typ](f: Rep[A] => Rep[C], g: Rep[B] => Rep[D]) =
      paircps_map(pair, f, g)

    def apply[X: Typ](k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] =
      paircps_apply(pair, k)

    def toPair: Rep[(A, B)] = paircps_toPair(pair)

    /**
     * for now we don't include other operations on pairCPS, we don't
     * seem to need them
     */
  }

  /**
   * interface level functions
   */
  def Pair[A: Typ, B: Typ](a: Rep[A], b: Rep[B]): Rep[PairCPS[A, B]]

  def paircps_map[A: Typ, B: Typ, C: Typ, D: Typ](
    pair: Rep[PairCPS[A, B]],
    f: Rep[A] => Rep[C],
    g: Rep[B] => Rep[D]): Rep[PairCPS[C, D]]

  def paircps_apply[A: Typ, B: Typ, X: Typ](
    pair: Rep[PairCPS[A, B]],
    k: (Rep[A], Rep[B]) => Rep[X]): Rep[X]

  def pair_conditional[A: Typ, B: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[PairCPS[A, B]],
    elsep: => Rep[PairCPS[A, B]]): Rep[PairCPS[A, B]]

  //def __ifThenElse[A: Typ, B: Typ](
  //  cond: Rep[Boolean],
  //  thenp: => Rep[PairCPS[A, B]],
  //  elsep: => Rep[PairCPS[A, B]]) = pair_conditional(cond, thenp, elsep)

  def paircps_toPair[A: Typ, B: Typ](pair: Rep[PairCPS[A, B]]): Rep[(A, B)]
}

trait PairCPSExp
    extends PairCPS
    with BaseExp
    with TupleOpsExp
    with IfThenElseExp
    with BooleanOpsExp
    with OptionOpsExp
    with ZeroValExp {

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

  /**
   * The wrapper acting as Rep[PairCPS[A]]
   */
  case class PairWrapper[A: Typ, B: Typ](p: PairCPS[A, B]) extends Def[PairCPS[A, B]]

  def Pair[A: Typ, B: Typ](a: Rep[A], b: Rep[B]) = PairWrapper(mkPair(a, b))



  def paircps_map[A: Typ, B: Typ, C: Typ, D: Typ](
    pair: Rep[PairCPS[A, B]],
    f: Rep[A] => Rep[C],
    g: Rep[B] => Rep[D]): Rep[PairCPS[C, D]] = pair match {
    case Def(PairWrapper(pair)) => PairWrapper(pair.map(f, g))
  }

  def paircps_apply[A: Typ, B: Typ, X: Typ](
    pair: Rep[PairCPS[A, B]],
    k: (Rep[A], Rep[B]) => Rep[X]): Rep[X] = pair match {
    case Def(PairWrapper(pair)) => pair(k)
  }

  /**
   * a 'conditional' pair
   * lifts conditional expressions to Option level
   *
   * Note: this implementation works only because we are
   * evaluating `thenp` and `elsep` here, and they are simple expressions
   * If they are blocks, the pattern match will fail.
   */
  def pair_conditional[A: Typ, B: Typ](
    cond: Rep[Boolean],
    thenp: => Rep[PairCPS[A, B]],
    elsep: => Rep[PairCPS[A, B]]
  ): Rep[PairCPS[A, B]] = (thenp, elsep) match { //stricting them here
    case (Def(PairWrapper(t)), Def(PairWrapper(e))) =>
      PairWrapper(conditional(cond, t, e))
  }

  def paircps_toPair[A: Typ, B: Typ](
      pair: Rep[PairCPS[A, B]]): Rep[(A, B)] = pair match {
    case Def(PairWrapper(pair)) => pair.toPair
  }
}
