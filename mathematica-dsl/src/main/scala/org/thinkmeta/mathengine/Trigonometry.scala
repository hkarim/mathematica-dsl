package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Trigonometry {

  val pi = SymbolNode("Pi")
  val π = pi

  def sin(theta: Operand) = FunctionNode("Sin", List(theta))

  def cos(theta: Operand) = FunctionNode("Cos", List(theta))

  def tan(theta: Operand) = FunctionNode("Tan", List(theta))

}