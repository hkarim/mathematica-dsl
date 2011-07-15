package org.thinkmeta.mathengine



/**
 *
 * @author hkarim
 * @since 6/5/11
 *
 */

trait Node
trait Operand extends Node
trait Operator extends Operand

trait Primitive[A] extends Operand {
  val value: A
}

case class SymbolNode(name: String) extends Operand


case class PrimitiveNode[A](value: A) extends Primitive[A]

case class FunctionNode[A <: Operand](function: String, args: List[A]) extends Operator

case class FoldApply[A <: Operand](function: Operand, args: List[A]) extends Operator
case class FunctionApply[A <: Operand](function: Operand, args: List[A]) extends Operand

