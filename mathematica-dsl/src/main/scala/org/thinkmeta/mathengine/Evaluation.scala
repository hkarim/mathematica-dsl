package org.thinkmeta.mathengine

import com.wolfram.jlink.{MathLink, LoopbackLink, MathLinkFactory, KernelLink}

/**
 *
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Evaluation {

  val kernel: KernelLink = {
    val kernelArgs =
      MathEngine.config.getProperty("mathematica.createKernelLink")
    val ml =
      MathLinkFactory.createKernelLink(kernelArgs)
    ml.discardAnswer()
    //ml.evaluate("foldApply[f_, args__] := Fold[#1[#2 /. {a___} -> a] &, f, args]")
    //ml.discardAnswer()
    ml

  }


  def eval(link: KernelLink, node: Operand) {
    node match {
      case SymbolNode(x) ⇒ link.putSymbol(x)
      case PrimitiveNode(x) ⇒ link.put(x.asInstanceOf[Any])
      case FunctionNode(name, args) ⇒
        link.putFunction(name, args.length)
        args.foreach(a ⇒ eval(link, a))
      case FoldApply(function, list) ⇒
        link.putFunction("foldApply", 2)
        eval(link, function)
        link.putFunction("List", list.length)
        list.foreach(a ⇒ eval(link, a))
      case FunctionApply(f, l) ⇒
        link.putNext(MathLink.MLTKFUNC)
        link.putArgCount(l.length)
        eval(link, f)
        l.foreach(a ⇒ eval(link, a))
    }
  }

  def expression(loop: LoopbackLink, node: Operand) {
    node match {
      case SymbolNode(x) ⇒ loop.putSymbol(x)
      case PrimitiveNode(x) ⇒ loop.put(x.asInstanceOf[Any])
      case FunctionNode(name, args) ⇒
        loop.putFunction(name, args.length)
        args.foreach(a ⇒ expression(loop, a))
      case FoldApply(function, list) ⇒
        loop.putFunction("foldApply", 2)
        expression(loop, function)
        loop.putFunction("List", list.length)
        list.foreach(a ⇒ expression(loop, a))
      case FunctionApply(f, l) ⇒
        loop.putNext(MathLink.MLTKFUNC)
        loop.putArgCount(l.length)
        expression(loop, f)
        l.foreach(a ⇒ expression(loop, a))
    }
  }

  def expression(node: Operand): String = {
    val loop = MathLinkFactory.createLoopbackLink
    expression(loop, node)
    val e = loop.getExpr
    loop.close()
    val result = kernel.evaluateToOutputForm(e, 72)
    val string = e.toString
    e.dispose()
    string
  }


  def evaluate(node: Operand) = {
    val math = kernel

    eval(math, node)
    //math.evaluate(expression(node))
    math.endPacket()
    math.waitForAnswer()
    val answer = math.getExpr
    //println(math.errorMessage())

    //math.close()

    answer.toString
  }

  def graphics(node: Operand, width: Int = 800, height: Int = 600, dpi: Int = 400, useFE: Boolean = true) = {
    val math = kernel

    //eval(math, node)
    math.evaluate(expression(node))
    math.endPacket()
    math.waitForAnswer()
    math.evaluateToImage(math.getExpr, width, height, dpi, useFE)
  }

  def done() {
    kernel.close()
  }


}