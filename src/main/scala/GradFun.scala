import breeze.linalg.DenseVector
import breeze.numerics._

class GradFun(
    f: (Vector[GradVal]=>Vector[GradVal]),
    dF: (Vector[DenseVector[Double]]=>Vector[DenseVector[Double]]),
    var ins: Option[Vector[GradVal]]=Option.empty,
    var out: Option[GradVal]=Option.empty,
    var generation: Int = 0
) extends F:
    def apply(ins: Vector[GradVal]): GradVal =
        this.generation = ins.map(_.generation).max
        val outs = this.f(ins).map(_.setCreator(this))
        val out = outs(0)
        this.ins = Some(ins)
        this.out = Some(out)
        out

    def backward(outGrads: Vector[DenseVector[Double]]): Vector[DenseVector[Double]] =
        // 2->1のとき、dF(values)は2つだが、outGradsは1つなのでここは水増しする
        println(this.f.toString())
        dF(ins.get.map(_.value)).zipAll(outGrads, DenseVector(0.0), outGrads(0))
                                .map((div, grad) => 
                                        // println("div "+div)
                                        // println("grad "+grad)
                                        grad * div
                                )