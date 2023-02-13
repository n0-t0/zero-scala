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

        val x = Vector(GradVal(DenseVector(2.0)))
        val a = squareFun(x)
        val b = squareFun(a)
        val c = squareFun(a)
        val y = addFun(b.concat(c))
        println(x.head.generation)
        println(a.head.generation)
        println(b.head.generation)
        println(c.head.generation)
        println(y.head.generation)
        y(0).backward()
        println(y.head.value)
        println(x.head.grad)
        