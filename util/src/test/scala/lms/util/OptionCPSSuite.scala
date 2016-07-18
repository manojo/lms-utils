package lms.util

import lms._
import lms.testutil.FileDiffSpec

import scala.lms.common._
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

trait OptionCPSProg
    extends OptionCPS
    with OrderingOps
    with PrimitiveOps
    with NumericOps
    with While {

  import OptionCPS._

  /**
   * NOTE: we use `conditional` instead of the classic
   * if then else sugar, because there is either the virtualized
   * version (which required Reps for everything) or classic
   * version (no Reps anywhere)
   */

  def singleConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional((in <= unit(3)),
      Some(in),
      None[Int]
    )
    c.toOption
  }

  def nestedConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional(
      in <= unit(3),
      conditional(in >= unit(1), Some(in), None[Int]),
      conditional(in >= unit(5), Some(in), None[Int])
    )
    c.toOption
  }

  /**
   * The same conditional expression twice
   * to make sure that no optimization collapses the conditional
   */
  def nestedConditional2(in: Rep[Int]): Rep[Option[Int]] = {
    val c = conditional(
      in == unit(3),
      Some(in),
      conditional(in == unit(3), Some(in), None[Int])
    )
    c.toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def mapSome(in: Rep[Int]): Rep[Option[Int]] = {
    val s = Some(in)
    s.map(x => x * unit(2)).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard. In particular the function passed
   * to map is not generated
   */
  def mapNone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None[Int]
    s.map(x => x * unit(2)).toOption
  }

  def mapConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), None[Int])
    s.map(_ * unit(2)).toOption
  }

  def mapConditional2(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), Some(in * unit(2)))
    s.map(_ * unit(3)).toOption
  }

  def mapNestedConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(
      in <= unit(3),
      conditional(in >= unit(1), Some(in), None[Int]),
      Some(in * unit(2))
    )
    s.map(_ * unit(3)).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def flatMapSome(in: Rep[Int]): Rep[Option[Int]] = {
    val s = Some(in)
    s.flatMap(x => Some(x * unit(2))).toOption
  }

  /**
   * should generate code where the notion of option
   * has disappeard
   */
  def flatMapNone(in: Rep[Int]): Rep[Option[Int]] = {
    val s = None[Int]
    s.flatMap(x => Some(x * unit(2))).toOption
  }

  def flatMapConditional(in: Rep[Int]): Rep[Option[Int]] = {
    val s = conditional(in <= unit(3), Some(in), None[Int])
    s flatMap { x =>
      conditional(
        x >= unit(1),
        Some(x * unit(5)),
        Some(x * unit(10))
      )
    } toOption
  }

  /**
   * tests whether overriding var_new and var_assign actually worked
   */
  def varOptionCPS(in: Rep[Int]): Rep[Option[Int]] = {
    var opt = mkSome(in)
    opt = readVar(opt) map (_ + 1)
    readVar(opt).toOption
  }

  /**
   * nested OptionCPS conversion to Option
   */
  def nestedOptionCPSToOption(in: Rep[Int]): Rep[Option[Option[Int]]] = {
    val opt = mkSome(mkSome(in))
    val optioned: Rep[OptionCPS[Option[Int]]] = opt.map(x => x.toOption)
    optioned.toOption

  }

  /**
   * nested zeroVal
   */
  def nestedZeroVal(in: Rep[Int]): Rep[Option[Option[Int]]] = {
    val opt = zeroVal[OptionCPS[OptionCPS[Int]]]
    opt.map(_.toOption).toOption
  }

  /**
   * tests whether overriding var_new and var_assign actually worked
   */
  def nestedVarOptionCPS(in: Rep[Int]): Rep[Option[Option[Int]]] = {
    var opt = mkSome(mkSome(in))
    readVar(opt).map(_.toOption).toOption
  }

  /**
   * tests whether overriding var_new and var_assign actually worked
   */
  def nestedVarOptionCPSAssign(in: Rep[Int]): Rep[Option[Option[Int]]] = {
    var opt = mkSome(mkSome(in))
    opt = mkSome(mkSome(in + 1))
    readVar(opt).map(_.toOption).toOption
  }

  /**
   * Option in an Option
   */
  def nestedOptionCPS(in: Rep[Int]): Rep[Option[Option[Int]]] = {

    val nestedOpt = option_conditional(in <= unit(3),
      mkSome(mkSome(in)),
      mkSome(mkSome(in + unit(1)))
    )
    nestedOpt.map(_.toOption).toOption
  }

  /**
   * var in a while
   */
  def varOptionCPSWhile(in: Rep[Int]): Rep[Option[Int]] = {
    var opt = mkNone[Int]
    var i = 0

    while (i < in) {
      val bla = readVar(opt).map(_ * 2)
      opt = bla
      i = i + 1
    }

    readVar(opt).toOption
  }

}

class OptionCPSSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `OptionCPS generate code with no diff` = {
    withOutFile(prefix + "optioncps") {
      new OptionCPSProg
          with OptionCPSExp
          with OrderingOpsExpOpt
          with PrimitiveOpsExpOpt
          with NumericOpsExpOpt
          with ImplicitOpsExp
          with WhileExp
          ///** this trait should be mixed in higher up */ with ArrayOpsExp
          ///** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new ScalaGenBase
            with OptionCPSGenBase
            with ScalaGenIfThenElse
            with ScalaGenBooleanOps
            with ScalaGenOrderingOps
            with ScalaGenEqual
            with ScalaGenVariables
            with ScalaGenOptionOps
            with ScalaGenPrimitiveOps
            with ScalaGenNumericOps
            with ScalaGenWhile {

          val IR: self.type = self
        }

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
        codegen.reset

        codegen.emitSource(nestedConditional2 _, "nestedConditional2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedConditional2 = compile(nestedConditional2)
        scala.Console.println(testcNestedConditional2(5))
        codegen.reset

        codegen.emitSource(mapSome _, "mapSome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapSome = compile(mapSome)
        scala.Console.println(testcMapSome(5))
        codegen.reset

        codegen.emitSource(mapNone _, "mapNone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapNone = compile(mapNone)
        scala.Console.println(testcMapNone(5))
        codegen.reset

        codegen.emitSource(mapConditional _, "mapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional = compile(mapConditional)
        scala.Console.println(testcMapConditional(5))
        codegen.reset

        codegen.emitSource(mapConditional2 _, "mapConditional2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapConditional2 = compile(mapConditional2)
        scala.Console.println(testcMapConditional2(5))
        codegen.reset

        codegen.emitSource(mapNestedConditional _, "mapNestedConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapNestedConditional = compile(mapNestedConditional)
        scala.Console.println(testcMapNestedConditional(5))
        codegen.reset

        codegen.emitSource(flatMapSome _, "flatMapSome", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapSome = compile(flatMapSome)
        scala.Console.println(testcFlatMapSome(5))
        codegen.reset

        codegen.emitSource(flatMapNone _, "flatMapNone", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapNone = compile(flatMapNone)
        scala.Console.println(testcFlatMapNone(5))
        codegen.reset

        codegen.emitSource(flatMapConditional _, "flatMapConditional", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapConditional = compile(flatMapConditional)
        scala.Console.println(testcFlatMapConditional(5))
        scala.Console.println(testcFlatMapConditional(3))
        scala.Console.println(testcFlatMapConditional(0))
        codegen.reset

        codegen.emitSource(varOptionCPS _, "varOptionCPS", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcVarOptionCPS = compile(varOptionCPS)
        scala.Console.println(testcVarOptionCPS(5))
        codegen.reset

        codegen.emitSource(nestedOptionCPSToOption _, "nestedOptionCPSToOption", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedOptionCPSToOption = compile(nestedOptionCPSToOption)
        scala.Console.println(testcNestedOptionCPSToOption(5))
        codegen.reset

        codegen.emitSource(nestedZeroVal _, "nestedZeroVal", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedZeroVal = compile(nestedZeroVal)
        scala.Console.println(testcNestedZeroVal(5))
        codegen.reset

        codegen.emitSource(nestedVarOptionCPS _, "nestedVarOptionCPS", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedVarOptionCPS = compile(nestedVarOptionCPS)
        scala.Console.println(testcNestedVarOptionCPS(5))
        codegen.reset

        codegen.emitSource(nestedVarOptionCPSAssign _, "nestedVarOptionCPSAssign", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedVarOptionCPSAssign = compile(nestedVarOptionCPSAssign)
        scala.Console.println(testcNestedVarOptionCPSAssign(5))
        codegen.reset

        codegen.emitSource(nestedOptionCPS _, "nestedOptionCPS", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcNestedOptionCPS = compile(nestedOptionCPS)
        scala.Console.println(testcNestedOptionCPS(5))
        codegen.reset

//        codegen.emitSource(varOptionCPSWhile _, "varOptionCPSWhile", new java.io.PrintWriter(System.out))
//        codegen.reset
//
//        val testcVarOptionCPSWhile = compile(varOptionCPSWhile)
//        scala.Console.println(testcVarOptionCPSWhile(6))
//        codegen.reset


      }
    }

    assertFileEqualsCheck(prefix + "optioncps")
  }
}
