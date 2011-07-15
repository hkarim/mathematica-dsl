package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Structure {

  def lst(elements: Operand*): FunctionNode[Operand] = {
    val l: List[Operand] =
      elements.toList.map {
        _ match {
          case l: List[Operand] ⇒ lst(l:_*)
          case o: Operand ⇒ o
          //case a: Any ⇒ PrimitiveNode(a)
        }
      }
    FunctionNode("List", l)
  }

  def Ξ(elements: Operand*) = lst(elements:_*)

}