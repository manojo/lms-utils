package lms.util

import lms._
import lms.util._
import lms.testutil.FileDiffSpec

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait EitherCPSProg
    extends EitherCPS
    with OrderingOps
    with PrimitiveOps
    with NumericOps {

  import EitherCPS._

  /**
   * just a single conditional expression
   */
  def singleConditional(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    val c = if (in <= unit(3)) mkLeft[Int, Int](unit(2))
            else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }

  /**
   * Let's nest the conditional in
   */
  def nestedConditional(in: Rep[Int]): Rep[Int] = {
    var res = unit(0)

    val c = if (in <= unit(3)) {
      if (in >= unit(1)) mkLeft[Int, Int](unit(2))
      else mkRight[Int, Int](unit(3))

    } else mkRight[Int, Int](unit(4))

    c.apply(l => res = l, r => res = r)
    res
  }

}

class EitherCPSSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `EitherCPS generate code with no diff` = {
    withOutFile(prefix + "eithercps") {
      new EitherCPSProg
          with EitherCPSExp
          with OrderingOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          ///** this trait should be mixed in higher up */ with ArrayOpsExp
          ///** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with EitherCPSGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables
            with ScalaGenPrimitiveOps
            with ScalaGenNumericOps { val IR: self.type = self }

        codegen.emitSource(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile(singleConditional)
        scala.Console.println(testcSingleConditional(5))
        scala.Console.println(testcSingleConditional(3))
        codegen.reset

        codegen.emitSource(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile(nestedConditional)
        scala.Console.println(testcNestedConditional(5))
        scala.Console.println(testcNestedConditional(3))
        scala.Console.println(testcNestedConditional(0))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "eithercps")
  }
}
