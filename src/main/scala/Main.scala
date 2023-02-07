import breeze.linalg._
import breeze.numerics._

object Main:
    def main(args: Array[String]): Unit =
        val f = GradFun(F.SQUARE, F.D_SQUARE)
        val input = GradVal(DenseVector(0.5))
        val output = F.zero_square(F.zero_exp(F.zero_square(input)))
        output.backward()
        println(input.grad)