package org.thinkmeta.mathengine

/**
 * 
 * @author hkarim
 * @since 6/7/11
 *
 */

trait Graphics {

  this: Structure ⇒

  val Green = SymbolNode("Green")
  val Orange = SymbolNode("Orange")
  val White = SymbolNode("White")
  val Pink = SymbolNode("Pink")
  val Blue = SymbolNode("Blue")

  val Thick = SymbolNode("Thick")
  val Thin = SymbolNode("Thin")

  val Large = SymbolNode("Large")
  val Medium = SymbolNode("Medium")
  val Small = SymbolNode("Small")
  val Tiny = SymbolNode("Tiny")

  val Plain = SymbolNode("Plain")
  val Italic = SymbolNode("Italic")
  val Bold = SymbolNode("Bold")

  val PlotStyle = SymbolNode("PlotStyle")
  val PlotRange = SymbolNode("PlotRange")
  val PlotPoints = SymbolNode("PlotPoints")
  val AxesLabels = SymbolNode("AxesLabels")
  val Axes = SymbolNode("Axes")
  val None = SymbolNode("None")
  val Mesh = SymbolNode("Mesh")
  val MaxRecursion = SymbolNode("MaxRecursion")
  val ColorFunction = SymbolNode("ColorFunction")
  val ColorData = SymbolNode("ColorData")
  val Boxed = SymbolNode("Boxed")

  val (φ, θ) = (SymbolNode("\\[Phi]"), SymbolNode("\\[Theta]"))

  val VertexStyle = SymbolNode("VertexStyle")
  val EdgeStyle = SymbolNode("EdgeStyle")


  def specularity(s: Operand, n: Operand) =
    FunctionNode("Specularity", List(s, n))

  def style(expr: Operand, options: Operand*) =
    FunctionNode("Style", List(expr) ++ options.toList)

  def directive(g: Operand*) =
    FunctionNode("Directive", g.toList)

  def opacity(a: Operand) =
    FunctionNode("Opacity", List(a))
  def opacity(a: Operand, color: Operand) =
    FunctionNode("Opacity", List(a, color))

  def graph(operands: Operand*) =
    FunctionNode("Graph", operands.toList)

  def parametricPlot3D(fx: Operand, fy: Operand, fz: Operand, u: Operand, v: Operand, options: Operand*) =
    options.toList match {
      case Nil ⇒ FunctionNode("ParametricPlot3D", List(lst(fx, fy, fz), u, v))
      case list   ⇒ FunctionNode("ParametricPlot3D", List(lst(fx, fy, fz), u, v) ++ list)
    }

  def sphericalPlot3D(r: Operand, θ: Operand, φ: Operand, options: Operand*) =
    options.toList match {
      case Nil ⇒ FunctionNode("SphericalPlot3D", List(r, θ, φ))
      case list   ⇒ FunctionNode("SphericalPlot3D", List(r, θ, φ) ++ list)
    }





}