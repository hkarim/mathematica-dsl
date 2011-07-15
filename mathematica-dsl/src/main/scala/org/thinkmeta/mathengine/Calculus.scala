package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/12/11
 *
 */

trait Calculus {

  def sum(function: Operand, options: Operand*) =
    FunctionNode("Sum", List(function) ++ options.toList)

}