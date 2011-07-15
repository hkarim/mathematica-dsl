package org.thinkmeta.mathengine


/**
 *
 * @author hkarim
 * @since 6/7/11
 *
 */


trait Arithmatic[A <: Operand] {


  def +(lhs: A)(rhs: A): A

  def -(lhs: A)(rhs: A): A = this.+(lhs)(this.unary_-(rhs))
  
  def unary_-(operand: A): A

  def *(lhs: A)(rhs: A): A
  
  def /(lhs: A)(rhs: A): A
  
  def power(lhs: A)(rhs: A): A

}

trait OperandArithmatic extends Arithmatic[Operand] {
  def +(lhs: Operand)(rhs: Operand): Operand = FunctionNode("Plus", List(lhs, rhs))
  def *(lhs: Operand)(rhs: Operand): Operand = FunctionNode("Times", List(lhs, rhs))
  def /(lhs: Operand)(rhs: Operand): Operand = FunctionNode("Divide", List(lhs, rhs))
  def power(lhs: Operand)(rhs: Operand): Operand = FunctionNode("Power", List(lhs, rhs))
  def unary_-(operand: Operand): Operand = FunctionNode("Minus", List(operand))





}