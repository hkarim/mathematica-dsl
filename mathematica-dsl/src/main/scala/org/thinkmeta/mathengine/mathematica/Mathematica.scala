package org.thinkmeta.mathengine.mathematica

import java.util.UUID
import org.thinkmeta.mathengine._


/**
 *
 * @author hkarim
 * @since 6/7/11
 *
 */

object Mathematica extends Core with Types with Structure with Trigonometry with Calculus with Graphics {

  implicit val mathematica =
    new Object with Functional with OperandArithmatic with Logic with Graph /*with Evaluation*/

  implicit def asSymbolOps(value: Symbol) = new MExpression(SymbolNode(value.name))
  implicit def asSymbol(value: Symbol) = SymbolNode(value.name)

  //implicit def asIntOps(value: Int) = new MExpression(asInt(value))
  implicit def asInt(value: Int) = PrimitiveNode(value)

  //implicit def asFloatOps(value: Float) = new MExpression(asFloat(value))
  implicit def asFloat(value: Float) = PrimitiveNode(value)

  //implicit def asLongOps(value: Long) = new MExpression(asLong(value))
  implicit def asLong(value: Long) = PrimitiveNode(value)

  //implicit def asDoubleOps(value: Double) = new MExpression(asDouble(value))
  implicit def asDouble(value: Double) = PrimitiveNode(value)

  //implicit def asStringOps(value: String) = new MExpression(asString(value))
  implicit def asString(value: String) = PrimitiveNode(value)

  //implicit def asRuleOps[A <: Operand, B <: Operand](tuple: (A, B)) = new MExpression(asRule(tuple))
  implicit def asRule[A <: Operand, B <: Operand](tuple: (A, B)) =
    FunctionNode("Rule", List(tuple._1, tuple._2))

  //implicit def asFunctionOps[B <: Operand](f: Operand ⇒ B)(implicit functional: Functional) = new MExpression(asFunction(f))
  implicit def asFunction[B <: Operand](f: Operand ⇒ B)(implicit functional: Functional) = {
    val x = SymbolNode("x" + UUID.randomUUID.toString.replaceAll("-", ""))
    functional.↦(x, f(x))
  }

  implicit def expression[A](value: A)(implicit ev: A ⇒ Operand) =
    new MExpression(ev.apply(value))
  implicit def core[A <: Operand](operand: A) = new MExpression(operand)

  def math[T](expression: Evaluation ⇒ T) = {
    implicit val evaluator = new Object with Evaluation
    val result = expression(evaluator)
    evaluator.done()
    result
  }

  def $(slot: Int) = FunctionNode("Slot", List(PrimitiveNode(slot)))
  val $1 = $(1)
  val $2 = $(2)
  val $3 = $(3)
  val $4 = $(4)
  val $5 = $(5)
  val $6 = $(6)
  val $7 = $(7)
  val $8 = $(8)
  val $9 = $(9)

  val _1 = $(1)
  val _2 = $(2)
  val _3 = $(3)
  val _4 = $(4)
  val _5 = $(5)
  val _6 = $(6)
  val _7 = $(7)
  val _8 = $(8)
  val _9 = $(9)

  class MExpression(operand: Operand) {

    def →(value: Operand) = FunctionNode("Rule", List(operand, value))

    def :>(value: Operand) = FunctionNode("RuleDelayed", List(operand, value))
    def ⧴(value: Operand) = FunctionNode("RuleDelayed", List(operand, value))

    def repall(value: Operand) = FunctionNode("ReplaceAll", List(operand, value))
    def ⧸∙(value: Operand) = FunctionNode("ReplaceAll", List(operand, value))
    def reprep(value: Operand) = FunctionNode("ReplaceRepeated", List(operand, value))
    def ⧸⧸∙(value: Operand) = FunctionNode("ReplaceRepeated", List(operand, value))
    def iff(value: Operand)(implicit functional: Functional) =
      functional.iff(operand, value)
    def ⧸⨟(value: Operand)(implicit functional: Functional) =
      functional.iff(operand, value)

    def flatten = FunctionNode("Flatten", List(operand))


    // Functional
    def /@(value: Operand)(implicit functional: Functional) =
      functional./@(operand, value)

    def ↦(value: Operand)(implicit functional: Functional) =
      functional.↦(operand, value)

    def :=(value: Operand)(implicit functional: Functional, evaluation: Evaluation) = {
      val result =  functional.:=(operand, value)
      new MExpression(result).evaluate
      result
    }

    def $(slot: Int) = FunctionNode("Slot", List(PrimitiveNode(slot)))
    def & = FunctionNode("Function", List(operand))



    // Pattern[x,Blank[]]
    def % = FunctionNode("Pattern", List(operand, FunctionNode("Blank", List.empty[Operand])))

    def %% = FunctionNode("Pattern", List(operand, FunctionNode("BlankSequence", List.empty[Operand])))

    def %%% = FunctionNode("Pattern", List(operand, FunctionNode("BlankNullSequence", List.empty[Operand])))

    def ~ = FunctionNode("Pattern", List(operand, FunctionNode("Blank", List.empty[Operand])))
    def ~(head: Operand) = FunctionNode("Pattern", List(operand, FunctionNode("Blank", List(head))))
    def ~~ = FunctionNode("Pattern", List(operand, FunctionNode("BlankSequence", List.empty[Operand])))
    def ~~~ = FunctionNode("Pattern", List(operand, FunctionNode("BlankNullSequence", List.empty[Operand])))

    // PatternTest[Pattern[x, Blank[]], NumberQ]
    def ?(test: Operand) = FunctionNode("PatternTest", List(operand, test))
    def ~?(test: Operand) = FunctionNode("PatternTest", List(this.~, test))
    def ~~?(test: Operand) = FunctionNode("PatternTest", List(this.~~, test))
    def ~~~?(test: Operand) = FunctionNode("PatternTest", List(this.~~~, test))

    def apply(value: Operand*)(implicit functional: Functional) =
      FunctionApply(operand, value.toList)





    // Arithmatic
    def +(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.+(operand)(rhs)

    def -(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.-(operand)(rhs)

    def unary_- (implicit arithmatic: Arithmatic[Operand]) = arithmatic.unary_-(operand)

    def *(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.*(operand)(rhs)
    def ∙(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.*(operand)(rhs)

    def /(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic./(operand)(rhs)

    def power(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.power(operand)(rhs)
    def ^(rhs: Operand)(implicit arithmatic: Arithmatic[Operand]) = arithmatic.power(operand)(rhs)

    //def sqrt(rhs: Operand)(implicit arithmatic: Arithmatic) = arithmatic.sqrt(operand)

    //def n(rhs: Operand)(implicit arithmatic: Arithmatic) = arithmatic.n(operand)

    // Logic
    def >(rhs: Operand)(implicit logic: Logic) = logic.>(operand, rhs)

    def >=(rhs: Operand)(implicit logic: Logic) = logic.>=(operand, rhs)
    def ≥(rhs: Operand)(implicit logic: Logic) = logic.>=(operand, rhs)

    def <(rhs: Operand)(implicit logic: Logic) = logic.<(operand, rhs)

    def <=(rhs: Operand)(implicit logic: Logic) = logic.<=(operand, rhs)
    def ≤(rhs: Operand)(implicit logic: Logic) = logic.<=(operand, rhs)

    def ＝(rhs: Operand)(implicit logic: Logic) = logic.＝(operand, rhs)
    def ≠(rhs: Operand)(implicit logic: Logic) = logic.≠(operand, rhs)
    def ⩶(rhs: Operand)(implicit logic: Logic) = logic.⩶(operand, rhs)

    // Graph

    def ●-●(rhs: Operand)(implicit graph: Graph) =
      graph.●-●(operand, rhs)

    def ●-►(rhs: Operand)(implicit graph: Graph) =
      graph.●-►(operand, rhs)

    // Evaluation
    def expression(implicit evaluation: Evaluation) = evaluation.expression(operand)
    def evaluate(implicit evaluation: Evaluation) = evaluation.evaluate(operand)
    def graphics(implicit evaluation: Evaluation) = evaluation.graphics(operand)

    def map[A <: Operand](f: Operand ⇒ A)(implicit functional: Functional) = {
      //println("map")
      val x = SymbolNode("x" + UUID.randomUUID.toString.replaceAll("-", ""))
      val function = functional.↦(x, f(x))
      val expr = functional./@(function,  operand)
      //println(expr)
      expr
    }

    def flatMap[A <: Operand](f: Operand ⇒ A)(implicit functional: Functional) = {
      //println("flatMap")
      val x = SymbolNode("x" + UUID.randomUUID.toString.replaceAll("-", ""))
      val function = functional.↦(x, f(x))
      val expr = functional./@(function,  operand)
      val flattened = FunctionNode("Flatten", List(expr))
      //println(flattened)
      flattened
    }

    def withFilter[A <: Operand](f: Operand ⇒ A)(implicit functional: Functional) = {
      val x = SymbolNode("x" + UUID.randomUUID.toString.replaceAll("-", ""))
      val filter = functional.↦(x, f(x))
      val function = FunctionNode("Select", List(operand, filter))
      //println(function)
      function
    }
  }

}