import breeze.linalg.DenseVector
import breeze.numerics._
import F._
import GradFun._

object Main:
    def main(args: Array[String]): Unit =
        // val f = GradFun(F.SQUARE, F.D_SQUARE)
        // val input = Vector(GradVal(DenseVector(0.5)))
        // val output = squareFun(expFun(squareFun(input)))

        // val add_input1 = Vector(GradVal(DenseVector(2.0)))
        // val add_input2 = Vector(GradVal(DenseVector(3.0)))
        // val output_square_add = addFun(squareFun(add_input1).concat(squareFun(add_input2)))
        // // output(0).backward()
        // // println(input.head.grad)
        // output_square_add(0).backward()
        // println(output_square_add.head.value)
        // println(output_square_add.head.grad)
        // println(add_input1.head.grad)
        // println(add_input2.head.grad)

        // val x = Vector(GradVal(DenseVector(3.0)))
        // var y = addFun(x.concat(x))
        // y(0).backward()
        // println(x.head.grad)
        // x.head.clearGrad()
        // y = addFun(addFun(x.concat(x)).concat(x))
        // y(0).backward()
        // println(x.head.grad)

        // val x = Vector(GradVal(DenseVector(2.0)))
        // val a = squareFun(x)
        // val b = squareFun(a)
        // val c = squareFun(a)
        // val y = addFun(b.concat(c))
        // println(x.head.generation)
        // println(a.head.generation)
        // println(b.head.generation)
        // println(c.head.generation)
        // println(y.head.generation)
        // y(0).backward()
        // println(y.head.value)
        // println(x.head.grad)

        // val x = Vector(GradVal(DenseVector(2.0)))
        extension (x: Vector[GradVal])
            def +(a: Vector[GradVal]): Vector[GradVal] =
                F.addFun(x ++ a)

            def -(a: Vector[GradVal]): Vector[GradVal] =
                F.addFun(x ++ (-:(a)))

            def *(a: Vector[GradVal]): Vector[GradVal] =
                F.mulFun(x ++ a)

            def ::+(a: Double): Vector[GradVal] =
                F.addFun(x :+ F.as_gradval(a))

            def ::-(a: Double): Vector[GradVal] =
                F.addFun(x :+ F.as_gradval(-a))

            def ::*(a: Double): Vector[GradVal] =
                F.mulFun(x :+ F.as_gradval(a))

            def ::/(a: Double): Vector[GradVal] =
                F.divFun(x :+ F.as_gradval(a))

            def ::**(a: Double): Vector[GradVal] =
                F.powFun(x :+ F.as_gradval(a))

            def getValue: Vector[DenseVector[Double]] = x.map(_.value)

        def -:(x: Vector[GradVal]): Vector[GradVal] = F.negFun(x)

            
        extension (a: Double)
            def +::(x: Vector[GradVal]): Vector[GradVal] =
                F.addFun(F.as_gradval(a) +: x)

            def -::(x: Vector[GradVal]): Vector[GradVal] =
                F.addFun(F.as_gradval(a) +: (-:(x)))

            def *::(x: Vector[GradVal]): Vector[GradVal] =
                F.mulFun(F.as_gradval(a) +: x)

            def /::(x: Vector[GradVal]): Vector[GradVal] =
                F.divFun(F.as_gradval(a) +: x)

        // val y = x ::+ 3.0
        // val z = 3.0 +:: x
        // println(y.getValue)
        // println(z.getValue)

        // val a = Vector(GradVal(DenseVector(3.0)))
        // val y = ((a ::* 2.0) ::+ 1.0)
        // y(0).backward()
        // println(y.head.value)
        // println(a.head.grad)

        // val a = Vector(GradVal(DenseVector(2.0)))
        // val b = -:(a)
        // println(b.getValue)
        // b(0).backward()
        // println(a.head.grad)

        // val a = Vector(GradVal(DenseVector(2.0)))
        // val b = 2.0 -:: x
        // val c = x ::- 1.0
        // println(b.getValue)
        // println(c.getValue)

        // val c = Vector(GradVal(DenseVector(2.0)))
        // val d = c ::** 3.0
        // println(d.getValue)

        val a = Vector(GradVal(DenseVector(1.0)))
        val b = Vector(GradVal(DenseVector(1.0)))
        val c = (0.26 *:: ((a ::** 2) + (b ::** 2))) - (0.48 *:: (a * b))
        c.head.backward()
        println(a.head.grad)
        println(b.head.grad)



