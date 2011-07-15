package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/17/11
 *
 */

trait Graph {


  def ●-●(lhs: Operand, rhs: Operand) =
    FunctionNode("UndirectedEdge", List(lhs, rhs))


  def ●-►(lhs: Operand, rhs: Operand) =
    FunctionNode("DirectedEdge", List(lhs, rhs))

}