package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/18/11
 *
 */

trait Core {

  val True = SymbolNode("True")
  val False = SymbolNode("False")
  val NumberQ = SymbolNode("NumberQ")

  def count(list: Operand, pattern: Operand) =
    FunctionNode("Count", List(list, pattern))

}