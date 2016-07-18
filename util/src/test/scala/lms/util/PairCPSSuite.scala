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
    with NumericOps
    with While {

  import PairCPS._

  /**
   * NOTE: we use `conditional` instead of the classic
   * if then else sugar, because there is either the virtualized
   * version (which required Reps for everything) or classic
   * version (no Reps anywhere)
   */

  /**
   * tests whether overriding var_new and var_assign actually worked
   */
  def varPairCPS(in: Rep[Int]): Rep[(Int, Int)] = {
    var p = mkPair(in, in + unit(1))
    p = readVar(p).map(_ + unit(3), _ + unit(2))
    readVar(p).toPair
  }

  def singleConditional(in: Rep[Int]): Rep[(Int, Int)] = {
    val c: PairCPS[Int, Int] = conditional(in <= unit(3),
      Pair(in, unit(2)),
      Pair(unit(0), unit(0))
    )
    c.toPair
  }

  def nestedConditional(in: Rep[Int]): Rep[(Int, Int)] = {
    val c = conditional(
      in <= unit(3),
      conditional(
        in >= unit(1),
        Pair(in, in + unit(1)),
        Pair(in + unit(2), in)
      ),
      conditional(
        in >= unit(5),
        Pair(in, in + unit(2)),
        Pair(in + unit(3), in)
      )
    )
    c.toPair
  }

  /**
   * should generate code where the notion of pair
   * has disappeard
   */
  def mapper(in: Rep[Int]): Rep[(Int, Int)] = {
    val s = Pair(in, in + unit(1))
    s.map(x => x * unit(2), x => x + unit(3)).toPair
  }

  /**
   * Nesting Rep's of PairCPS
   */
  def nestedPairs(in: Rep[Int]): Rep[((Int, Int), Int)] = {
    val s = mkPair(
              mkPair(in, in + unit(1)),
              in + unit(2)
            )

    val mapped = s.map(p => p.toPair, i => i)
    mapped.toPair
  }

  /**
   * Nesting pairs in conditionals
   */
  def nestedPairConditional(in: Rep[Int]): Rep[(Int, Int)] = {

    val s: PairCPS[PairCPS[Int, Int], Int] = PairCPS.conditional(in <= unit(5),
      Pair(mkPair(in, in + unit(2)), unit(3)),
      Pair(mkPair(in, in + unit(1)), unit(3))
    )

    val mapped = s.map(p => p.apply((p1, p2) => p1 + p2), x => x)
    mapped.toPair
  }

  /**
   * We grow the abstraction until reaching a full zip
   *
   * Attempt 1: (a, b), streams of 5. dot product style
   *
   */
  def nestedPairsWhile(a: Rep[Int], b: Rep[Int]): Rep[Int] = {

    //val xs = rangeStream(a, unit(5))
    //val ys = rangeStream(b, unit(5))
    //val dotted = (xs zip ys).toFold //.map(pair => pair.apply((x, y) => x * y))
    //dotted.apply[List[Int]](List[Int](), (acc, x) => acc ++ List(x.apply((a, b) => a + b)))

    var i = a
    var j = b

    var sum = 0

    /**
     * serves as marker whether to pull from left or right
     */
    var aIsDefined = false
    var tmpA = zeroVal[Int]

    /** while (!atEnd) */
    while (!((i > unit(5) && !aIsDefined) || j > unit(5))) {

      var abIsDefined = unit(false)
      var elems = zeroVal[PairCPS[Int, Int]]

      if (aIsDefined) {

        elems = mkPair(readVar(tmpA), readVar(j))

        j = j + unit(1)
        abIsDefined = unit(true)
        aIsDefined = unit(false)

      } else {
        tmpA = i
        i = i + unit(1)
        aIsDefined = unit(true)
      }

      /** combine */
      if (abIsDefined) {
        sum = sum + (readVar(elems)._1 + readVar(elems)._2)
      }
    }
    sum
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
          with WhileExp
          // /** this trait should be mixed in higher up */ with ArrayOpsExp
          // /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
          with PairCPSGenBase
          with ScalaGenIfThenElse
          with ScalaGenBooleanOps
          with ScalaGenOrderingOps
          with ScalaGenEqual
          with ScalaGenVariables
          with ScalaGenTupleOps
          with ScalaGenPrimitiveOps
          with ScalaGenNumericOps
          with ScalaGenWhile { val IR: self.type = self }

        codegen.emitSource(varPairCPS _, "varPairCPS", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcVarPairCPS = compile(varPairCPS)
        scala.Console.println(testcVarPairCPS(5))
        codegen.reset

        codegen.emitSource(singleConditional _, "singleConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcSingleConditional = compile(singleConditional)
        scala.Console.println(testcSingleConditional(5))
        scala.Console.println(testcSingleConditional(2))
        codegen.reset

        codegen.emitSource(nestedConditional _, "nestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional = compile(nestedConditional)
        scala.Console.println(testcNestedConditional(5))
        scala.Console.println(testcNestedConditional(4))
        scala.Console.println(testcNestedConditional(0))
        scala.Console.println(testcNestedConditional(3))
        codegen.reset

        codegen.emitSource(mapper _, "mapper", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapper = compile(mapper)
        scala.Console.println(testcMapper(5))
        codegen.reset

        codegen.emitSource(nestedPairs _, "nestedPairs", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedPairs = compile(nestedPairs)
        scala.Console.println(testcNestedPairs(5))
        codegen.reset

        codegen.emitSource(nestedPairConditional _, "nestedPairConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedPairConditional = compile(nestedPairConditional)
        scala.Console.println(testcNestedPairConditional(5))
        scala.Console.println(testcNestedPairConditional(3))
        codegen.reset

        codegen.emitSource2(nestedPairsWhile _, "nestedPairsWhile", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedPairsWhile = compile2(nestedPairsWhile)
        scala.Console.println(testcNestedPairsWhile(1, 1))
        codegen.reset
      }
    }

    assertFileEqualsCheck(prefix + "paircps")
  }
}
