import breeze.linalg.DenseVector
import breeze.numerics._

trait F

object F:
    def numDiff(x: GradVal, f: (GradVal=>GradVal), h: Double=0.0001): GradVal =
        (f(GradVal(x.value+h)) - f(GradVal(x.value-h))) / (2.0*h)

    def uniOP(ins: Vector[GradVal], f: (GradVal => GradVal)): Vector[GradVal] =
        ins match {
            case Vector(gv) => Vector(f(gv))
            case _ => throw new Exception("invalid input")
        }
    def uniDOP(ins: Vector[DenseVector[Double]], f: (DenseVector[Double] => DenseVector[Double])): Vector[DenseVector[Double]] =
        ins match {
            case Vector(gv) =>
                println("gv: "+gv)
                Vector(f(gv))
            case _ => throw new Exception("invalid input")
        }
    def binOP(ins: Vector[GradVal], f: (GradVal, GradVal) => GradVal): Vector[GradVal] =
        ins match {
            case Vector(gv1, gv2) => Vector(f(gv1, gv2))
            case _ => throw new Exception("invalid input")
        }
    def binDOP(ins: Vector[DenseVector[Double]], f: DenseVector[Double] => (DenseVector[Double], DenseVector[Double])): Vector[DenseVector[Double]] =
        ins match {
            case Vector(gv) =>
                val (fdv1, fdv2) = f(gv)
                Vector(fdv1, fdv2)
            case _ => throw new Exception("invalid input")
        }

    final val SQUARE: (Vector[GradVal]=>Vector[GradVal]) =
        uniOP(_, gv=> GradVal(gv.value * gv.value))
    final val D_SQUARE: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        uniDOP(_, vec => vec * 2.0)

    final val EXP: (Vector[GradVal]=>Vector[GradVal]) =
        uniOP(_, gv => GradVal(exp(gv.value)))
    final val D_EXP: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        uniDOP(_, vec => exp(vec))

    final val ADD: (Vector[GradVal]=>Vector[GradVal]) =
        binOP(_, (gv1, gv2) => GradVal(gv1.value + gv2.value))
    final val D_ADD: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        binDOP(_, vec => (vec, vec))

    def squareFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.SQUARE, F.D_SQUARE)(ins))
    def expFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.EXP, F.D_EXP)(ins))
    def addFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.ADD, F.D_ADD)(ins))