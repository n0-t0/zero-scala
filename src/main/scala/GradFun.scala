import breeze.linalg.DenseVector
import breeze.numerics._

class GradFun(
    f: (Vector[GradVal]=>Vector[GradVal]),
    dF: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]),
    var ins: Option[Vector[GradVal]]=Option.empty,
    var out: Option[GradVal]=Option.empty
) extends F:
    def apply(ins: Vector[GradVal]): GradVal =
        val outs = this.f(ins).map(_.setCreator(this))
        val out = outs(0)
        this.ins = Some(ins)
        this.out = Some(out)
        out

    def backward(outGrads: Vector[DenseVector[Double]]): Vector[DenseVector[Double]] =
        dF(outGrads).zip(ins.get).map((grad, in) => grad * in.value)