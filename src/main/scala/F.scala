import breeze.linalg._
import breeze.numerics._

trait F

object F:
    final val SQUARE: (GradVal=>GradVal) = gv => GradVal(gv.value * gv.value, Option.empty)
    final val EXP: (GradVal=>GradVal) = gv => GradVal(exp(gv.value), Option.empty)
    final val D_SQUARE: (GradVal=>GradVal) = gv => GradVal(gv.value * 2.0, Option.empty)
    final val D_EXP: (GradVal=>GradVal) = gv => GradVal(exp(gv.value), Option.empty)
    final val ADD: (GradVal=>GradVal=>GradVal) = gv1 => gv2 => GradVal(gv1.value + gv2.value, Option.empty)

    def numDiff(x: GradVal, f: (GradVal=>GradVal), h: Double=0.0001): GradVal =
        (f(GradVal(x.value+h, Option.empty)) - f(GradVal(x.value-h, Option.empty))) / (2.0*h)
    def zero_square(in: GradVal): GradVal =
        GradFun(F.SQUARE, F.D_SQUARE).forward(in)
    def zero_exp(in: GradVal): GradVal =
        GradFun(F.EXP, F.D_EXP).forward(in)
    def zero_add(in1: GradVal, in2: GradVal): GradVal =
        GradFun(F.ADD(in1), F.D_EXP).forward(in2)