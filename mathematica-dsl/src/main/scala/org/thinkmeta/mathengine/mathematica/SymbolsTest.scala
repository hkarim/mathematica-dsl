package org.thinkmeta.mathengine.mathematica

import java.io.FileOutputStream
import org.thinkmeta.mathengine._


/**
 *
 * @author hkarim
 * @since 6/5/11
 *
 */

object SymbolsTest {
  def show[T](a: T){println(a)}

  def write(path: String, bytes: Array[Byte]) {
    val os = new FileOutputStream(path)
    os.write(bytes)
    os.flush()
    os.close()
  }

  def mathematica() {

    import Mathematica._

    val (x, y) = ('x, 'y)
    val f = 'f
    val (a, b, c, w) = ('a, 'b, 'c, 'w)

    math { e ⇒
      val f1 = (x ↦ (x ^ 2)) /@ Ξ(a, b) ⧸∙ (a → b) ⧸∙ (b → 1)
      println(f1.evaluate(e))
    }

    math { e ⇒
      val f2 = ((_: Operand) + 1) /@ Ξ(a, b) ⧸∙ (a → b) ⧸∙ (b → 1)
      //println(f2.evaluate(e)
    }

    math { e ⇒
      def puref(a: Operand): Operand = a + 1
      val f3 = (puref(_)) /@ Ξ(a, b) ⧸∙ (a → b) ⧸∙ (b → 1)
      println(f3.evaluate(e))
    }

    math { e ⇒
      val simple =
        for {
          x ← Ξ(1, 2, 3, 4) if x > 3
          y ← x + 1
        } yield y

      //println(simple.expression)

      val e =
        for {
          l ← Ξ(1, 2, 3)
          m ← Ξ(4, 5, 6) if m + l > 5
          n ← Ξ(7, 8, 9) if n < m + l
        } yield l + m + n
      //println(e)
      //println(e.expression)
      //println(e.evaluate)
    }




    /*
    ParametricPlot3D[ {
      Sin[u] Sin[v] + 0.05 Cos[20 v],
      Cos[u] Sin[v] + 0.05 Cos[20 u],
      Cos[v]},
      {u, -\[Pi], \[Pi]}, {v, -\[Pi], \[Pi]}, MaxRecursion -> 4,
      PlotStyle -> {Orange, Specularity[White, 10]}, Axes -> None,
      Mesh -> None]
     */
    /*val (u, v) = (SymbolNode("u"), SymbolNode("v"))
    val fx = (sin(u) * sin(v)) + (0.05 * cos(20 * v))
    val fy = (cos(u) * sin(v)) + (0.05 * cos(20 * v))
    val fz = cos(v)
    val xaxis = lst(u, -π, π)
    val yaxis = lst(v, -π, π)

    val g =
      parametricPlot3D(
        (sin(u) * sin(v)) + (0.05 * cos(20 * v)),
        (cos(u) * sin(v)) + (0.05 * cos(20 * v)),
        cos(v),
        lst(u, -pi, pi),
        lst(v, -pi, pi),
        MaxRecursion → 4,
        PlotStyle → lst(Green, specularity(White, 10)),
        Axes → None,
        Mesh → None)


    println(g.expression)
    val image = g.graphics
    write("result.png", image)
    println()*/


    /*val e1 = lst(6, -7, 3, 2, -1, -2) repall( (x.% iff (x < 0)) → w)
    println(e1.expression)*/

    math { implicit e ⇒ {

      f(x~mInteger) := ( (x^2) + 1)
      f(x~mReal) := ( (x^3) + 1)
      f(x~, y~) := (x + y)

      println("f(4):" + f(4).evaluate)
      println("f(4.1):" + f(4.1).evaluate)
      println("f(2,3):" + f(2,3).evaluate)
    }}

    math { implicit e ⇒ {
      val (xs, x, y, ys) = ('xs, 'x, 'y, 'ys)
      val bubbleSort = 'bubbleSort
      bubbleSort(Ξ(xs~~~, x~, y~, ys~~~)) := bubbleSort(Ξ(xs, y, x, ys)) ⧸⨟ (x ≥ y)
      println(bubbleSort(lst(3, 2, 1)).evaluate)
    }}

    math { e ⇒
      val (f, x, y, ys) = ('f, 'x, 'y, 'ys)
      val l = Ξ(1, 2, 3, 4) ⧸∙ ( Ξ(x~, y~, ys~~~) → Ξ(ys) )
      show(l.evaluate(e))
    }


  }

  def native() {
    val e =
      for {
        l ← List(1, 2, 3)
        m ← List(4, 5, 6) if m + l > 5
        n ← List(7, 8, 9) if n < m + l
      } yield l + m + n

    println(e.mkString("{", ", ", "}"))
  }

  def main(args: Array[String]) {

    //native()
    mathematica()

  }

}