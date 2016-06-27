package lms.util

import lms._
import lms.testutil.FileDiffSpec

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait PairCPSProg
    extends PairCPS
    with OrderingOps
    with PrimitiveOps
    with NumericOps {

  import PairCPS._

  /**
   * NOTE: we use `conditional` instead of the classic
   * if then else sugar, because there is either the virtualized
   * version (which required Reps for everything) or classic
   * version (no Reps anywhere)
   */

  def singleConditional(in: Rep[Int]): Rep[(Int, Int)] = {
    val c = conditional(in <= unit(3),
      mkPair(in, unit(2)),
      mkPair(unit(0), unit(0))
    )
    c.toPair
  }

  def nestedConditional(in: Rep[Int]): Rep[(Int, Int)] = {
    val c = conditional(
      in <= unit(3),
      conditional(
        in >= unit(1),
        mkPair(in, in + unit(1)),
        mkPair(in + unit(2), in)
      ),
      conditional(
        in >= unit(5),
        mkPair(in, in + unit(2)),
        mkPair(in + unit(3), in)
      )
    )
    c.toPair
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def mapper(in: Rep[Int]): Rep[(Int, Int)] = {
    val s = mkPair(in, in + unit(1))
    s.map(x => x * unit(2), x => x + unit(3)).toPair
  }

}

class PairCPSSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `PairCPS generate code with no diff` = {
    withOutFile(prefix + "paircps") {
      new PairCPSProg
          with PairCPSExp
          with OrderingOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables
            with ScalaGenTupleOps
            with ScalaGenPrimitiveOps
            with ScalaGenNumericOps { val IR: self.type = self }

        codegen.emitSource(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile(singleConditional)
        scala.Console.println(testcSingleConditional(5))
        codegen.reset

        codegen.emitSource(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile(nestedConditional)
        scala.Console.println(testcNestedConditional(5))
        codegen.reset

        codegen.emitSource(mapper _, "mapper", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapper = compile(mapper)
        scala.Console.println(testcMapper(5))
        codegen.reset
      }
    }

    assertFileEqualsCheck(prefix + "paircps")
  }
}
