package org.thinkmeta.mathengine

/**
 *
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Logic {

  def >(lhs: Operand, rhs: Operand) = FunctionNode("Greater", List(lhs, rhs))

  def >=(lhs: Operand, rhs: Operand) = FunctionNode("GreaterEqual", List(lhs, rhs))
  def ≥(lhs: Operand, rhs: Operand) = FunctionNode("GreaterEqual", List(lhs, rhs))

  def <(lhs: Operand, rhs: Operand) = FunctionNode("Less", List(lhs, rhs))

  def <=(lhs: Operand, rhs: Operand) = FunctionNode("LessEqual", List(lhs, rhs))
  def ≤(lhs: Operand, rhs: Operand) = FunctionNode("LessEqual", List(lhs, rhs))

  def ＝(lhs: Operand, rhs: Operand) = FunctionNode("Equal", List(lhs, rhs))
  def ≠(lhs: Operand, rhs: Operand) = FunctionNode("Unequal", List(lhs, rhs))
  def ⩶(lhs: Operand, rhs: Operand) = FunctionNode("SameQ", List(lhs, rhs))


}