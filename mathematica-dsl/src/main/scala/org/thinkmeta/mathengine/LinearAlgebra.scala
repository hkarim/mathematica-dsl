package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 7/27/11
 *
 */

trait LinearAlgebra {

  def dot(lhs: Operand, rhs: Operand) = FunctionNode("Dot", List(lhs, rhs))

}