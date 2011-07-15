package org.thinkmeta.mathengine


/**
 * 
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Functional {

  def /@(lhs: Operand, rhs: Operand) = FunctionNode("Map", List(lhs, rhs))

  def ↦(lhs: Operand, rhs: Operand) = FunctionNode("Function", List(lhs, rhs))

  def fold(function: Operand, initial: Operand, arg: Operand) =
    FunctionNode("FoldApply", List(function, initial, arg))

  def :=(lhs: Operand, rhs: Operand) = FunctionNode("SetDelayed", List(lhs, rhs))
  def iff(lhs: Operand, rhs: Operand) = FunctionNode("Condition", List(lhs, rhs))

}