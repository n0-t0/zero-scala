import breeze.linalg.DenseVector
import breeze.numerics._
import spire.std.unit
import breeze.stats.bincount

trait F

object F:
    def as_gradval(obj: Any): GradVal = {
        return obj match {
            case gv: GradVal => gv
            case dv: DenseVector[Double] => GradVal(dv)
            case d: Double => GradVal(DenseVector(d))
            case _ => throw new Exception("Not supported type")
        }
    }

    def numDiff(x: GradVal, f: (GradVal=>GradVal), h: Double=0.0001): GradVal =
        (f(GradVal(x.value+h)) - f(GradVal(x.value-h))) / (2.0*h)

    def uniOP(ins: Vector[GradVal], f: (GradVal => GradVal)): Vector[GradVal] =
        ins match {
            case Vector(gv) => Vector(f(gv))
            case _ => throw new Exception("invalid input")
        }
    def uniDOP(ins: Vector[DenseVector[Double]], f: (DenseVector[Double] => DenseVector[Double])): Vector[DenseVector[Double]] =
        ins match {
            case Vector(gv) => Vector(f(gv))
            case _ => throw new Exception("invalid input")
        }

    def binOP(
        ins: Vector[GradVal],
        f: (GradVal, GradVal) => GradVal
    ): Vector[GradVal] =
        ins match {
            case Vector(gv1, gv2) => Vector(f(gv1, gv2))
            case _ => throw new Exception("invalid input")
        }

    def binDOP(
        ins: Vector[DenseVector[Double]],
        f1: DenseVector[Double] => DenseVector[Double],
        f2: DenseVector[Double] => DenseVector[Double]
    ): Vector[DenseVector[Double]] =
        ins match {
            case Vector(gv1, gv2) => Vector(f1(gv1), f2(gv2))
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
        binDOP(_, dv => DenseVector(1.0), dv => DenseVector(1.0))

    final val MUL: (Vector[GradVal]=>Vector[GradVal]) =
        binOP(_, (gv1, gv2) => GradVal(gv1.value * gv2.value))

    final val D_MUL: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        ins => ins match {
            case Vector(gv1, gv2) => Vector(gv2, gv1)
            case _ => throw new Exception("invalid input")
        }

    final val NEG: (Vector[GradVal]=>Vector[GradVal]) =
        uniOP(_, gv => GradVal(-gv.value))
    final val D_NEG: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        uniDOP(_, vec => DenseVector(-1.0))

    final val SUB: (Vector[GradVal]=>Vector[GradVal]) =
        binOP(_, (gv1, gv2) => GradVal(gv1.value - gv2.value))
    final val D_SUB: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        binDOP(_, dv => DenseVector(1.0), dv => DenseVector(-1.0))

    final val DIV: (Vector[GradVal]=>Vector[GradVal]) =
        binOP(_, (gv1, gv2) => GradVal(gv1.value / gv2.value))
    final val D_DIV: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        ins => ins match {
            case Vector(gv1, gv2) => Vector((DenseVector.ones[Double](gv2.length)/gv2), -gv1 / (gv2*gv2))
            case _ => throw new Exception("invalid input")
        }

    final val POW: (Vector[GradVal]=>Vector[GradVal]) =
        binOP(_, (gv1, gv2) => GradVal(breeze.numerics.pow(gv1.value, gv2.value)))
    final val D_POW: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]) =
        ins => ins match {
            case Vector(gv1, gv2) => Vector(gv2 * breeze.numerics.pow(gv1, gv2-DenseVector(1.0)), breeze.numerics.pow(gv1, gv2) * log(gv1))
            case _ => throw new Exception("invalid input")
        }

    def squareFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.SQUARE, F.D_SQUARE)(ins))

    def expFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.EXP, F.D_EXP)(ins))

    def addFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.ADD, F.D_ADD)(ins))
    
    def mulFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.MUL, F.D_MUL)(ins))
    
    def negFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.NEG, F.D_NEG)(ins))

    def subFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.SUB, F.D_SUB)(ins))
    
    def divFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.DIV, F.D_DIV)(ins))

    def powFun(ins: Vector[GradVal]): Vector[GradVal] =
        Vector(GradFun(F.POW, F.D_POW)(ins))
