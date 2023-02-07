import breeze.linalg._
import breeze.numerics._

case class GradVal(value: DenseVector[Double], var grad: Option[DenseVector[Double]]=Option.empty, var creator: Option[GradFun]= Option.empty) {
    def +(gv2: GradVal): GradVal = GradVal((this.value+gv2.value), Option.empty)
    def -(gv2: GradVal): GradVal = GradVal((this.value-gv2.value), Option.empty)
    def *(gv2: GradVal): GradVal = GradVal((this.value*gv2.value), Option.empty)
    def /(gv2: GradVal): GradVal = GradVal((this.value-gv2.value), Option.empty)
    def /(a: Double): GradVal = GradVal((this.value/a), Option.empty)
    def +(a: Double): GradVal = GradVal((this.value+a), Option.empty)
    def -(a: Double): GradVal = GradVal((this.value-a), Option.empty)
    def *(a: Double): GradVal = GradVal((this.value*a), Option.empty)
    def setCreator(f: GradFun): GradVal =
        GradVal(this.value, this.grad, Some(f))
    def backward(): Unit =
        if (this.grad.isEmpty)
            this.grad = Some(DenseVector[Double](1.0))
        var f = this.creator.get
        var fs = List[GradFun](f)
        while !(fs.isEmpty) do
            f = fs.last
            fs = fs.dropRight(1)
            val inVal = f.in.get
            val outVal = f.out.get
            inVal.grad = Some(f.backward(outVal.grad.get))
            if (inVal.creator.isDefined)
                fs = fs.appended(inVal.creator.get)
}