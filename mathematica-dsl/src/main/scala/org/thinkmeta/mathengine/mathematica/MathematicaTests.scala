package org.thinkmeta.mathengine.mathematica

import org.thinkmeta.mathengine._
import java.io.FileOutputStream

/**
 *
 * @author hkarim
 * @since 6/7/11
 *
 */

object MathematicaTests {

  import Mathematica._

  def show[T](a: T){println(a)}
  def write(path: String, bytes: Array[Byte]) {
    val os = new FileOutputStream(path)
    os.write(bytes)
    os.flush()
    os.close()
  }

  val (x, y, a, i) = ('x, 'y, 'a, 'i)

  val addxy =
    x ↦ {
      y ↦ {
        x + y
      }
    }


  val tri = (sin(a) ^ 2) + (cos(a) ^ 2)

  def algo(i: Int) = math {
    (x ↦ (x + 1)).apply(i).evaluate(_)
  }

  def patterns() {
    /*
    {1, 2, 3} /. {
      {a_, b_} :> a + b,
      {a_, b_, c_} :> a + b + c}
     */
    val (a, b, c) = ('a, 'b, 'c)

    /*
    λx.x
    λt.λf.t
    λt.λf.f

     */

    /*val result =
      Ξ(1, 2, 3) ⧸∙ Ξ(
        Ξ(a~, b~)     ⧴ (a+b),
        Ξ(a~, b~, c~) ⧴ (a+b+c))

    println(result.evaluate)

    // f[2 x y] + f[x y] //. f[a_ b_] :> f[a] + f[b]
    val (f, x, y) = ('f, 'x, 'y)
    val r2 = (f(2∙x∙y) + f(x∙y)) ⧸⧸∙ (f( (a~)∙(b~) ) ⧴ (f(a) + f(b)))
    println(r2.evaluate)*/
    math {
      implicit e ⇒ {
        val x = 'x
        (x ^ 2) ⧸⨟ (x < 4)
      }
    }
    math {
      implicit e ⇒ {
        val (f, g, x, a, b, c) = ('f, 'g, 'x, 'a, 'b, 'c)

        // {a, b, c} /. x_ → x + 1
        val result = Ξ(a, b, c) ⧸∙ ((x~) → (x + 1))
        show(result.evaluate)


        //g[x_?NumberQ] := (x^2) /; (x < 4)
        g(x~?NumberQ) := (x ^ 2) ⧸⨟ (x < 4)
        show(g(2).evaluate)

        //g[x_] := (x^3) /; (x > 4)
        g(x~) := (x ^ 3) ⧸⨟ (x > 4)
        show(g(5).evaluate)

        //f[a_, b_, c_] := a + b + c
        f(a~, b~, c~) := a + b + c
        show(f(1,2,3).evaluate)

        //f[{a_, b_}, c_] := a + b + c
        f(Ξ(a~, b~), c~) := a + b + c
        show(f(Ξ(1,2),3).evaluate)

        show(count(Ξ(1, 2, 3, 4, 5, 6, 7), (x~) ⧸⨟ (x > 5)).evaluate)
      }
    }

  }

  def renderSphericalPlot3D() {
    /*
    SphericalPlot3D[
     1 + Sin[5 \[Phi]]/5, {\[Theta], 0, Pi}, {\[Phi], 0, 2 Pi},
     PlotStyle -> Directive[Orange, Opacity[0.7], Specularity[White, 10]],
     Mesh -> None, PlotPoints -> 30]
   */
    math {
      implicit e ⇒ {
        val g =
          sphericalPlot3D(
            1 + sin(5 ∙ φ) / 5, Ξ(θ, 0, π), Ξ(φ, 0, 2 ∙ π),
            PlotStyle → directive(Orange, opacity(0.7), specularity(White, 10)),
            Mesh → None, PlotPoints → 30)
        show(g.expression)
        val bytes = g.graphics
        write("g1.png", bytes)
      }
    }
  }



  def pureFunction() {
    math {
      implicit e ⇒ {
        val f = (_1 + _2 &)
        println(f(1, 2).evaluate)
      }
    }
  }


  def sphericalPlot2() {
    math { // do it in Mathematica
      implicit e ⇒ { // pass the evalautor where required
        // SphericalPlot3D[
        //   1+Sin[5 φ] Sin[10 θ]/10,{θ,0,π},{φ,0,2π},
        //   ColorFunction → (ColorData["Rainbow"][#6]&),
        //   Mesh → None,PlotPoints → 25,Boxed → False,Axes → False]
        val g =
          sphericalPlot3D(
            1 + sin(5 * φ) * sin(10 * θ) / 10, Ξ(θ, 0, π), Ξ(φ, 0, 2 * π),
            ColorFunction → ((ColorData("Rainbow") apply (_6)) &),
            Mesh → None, PlotPoints → 25, Boxed → False, Axes → False)
        show(g.expression)
        write("g3.png", g.graphics)
      }  // release the kernel
    }
  }

  def linearAlgebra() {
    math {
      implicit e ⇒ {
        val scalar  = lst(1,2,3) ∙ lst(3,4,5)
        println(scalar.evaluate) // -→ 26
      }
    }
  }


  def main(args: Array[String]) {
    //renderSphericalPlot3D()
    //patterns()
    //someGraph()
    //pureFunction()
    //sphericalPlot2()
    linearAlgebra()
  }

}