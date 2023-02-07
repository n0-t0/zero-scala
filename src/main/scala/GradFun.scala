import breeze.linalg.DenseVector
import breeze.numerics._

class GradFun(f: (GradVal=>GradVal), dF: (GradVal=>GradVal), var ins: Option[Vector[GradVal]]=Option.empty, var out: Option[Vector[GradVal]]=Option.empty) extends F:
    def forward(ins: Vector[GradVal]): Vector[GradVal] =
        this.ins = Some(ins)
        val outs = for {in <- ins}
        yield this.f(in).setCreator(this)
        outs
            // def forward(in: GradVal): GradVal =
    //     this.in = Some(in)
    //     val out = this.f(in).setCreator(this)
    //     this.out = Some(out)
    //     out
        // val out = this.f(in).setCreator(this)
        // this.out = Some(out)
        // out

    // def backward(grad: DenseVector[Double]): DenseVector[Double] =
    //     this.in match {
    //         case Some(value) => this.dF(value).value * grad
    //         case None => throw new Exception("value not found")
    //     }