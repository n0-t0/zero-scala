import breeze.linalg.DenseVector
import breeze.numerics._

case class GradVal(
    value: DenseVector[Double],
    var grad: Option[DenseVector[Double]]=Option.empty,
    var creator: Option[GradFun]= Option.empty
) {
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
        import scala.collection.mutable.Stack
        var f = this.creator.getOrElse(throw new Exception("No creator"))
        // val outGrad = this.grad.getOrElse(DenseVector[Double](1.0))
        var fStack = Stack[GradFun](f)
        while !(fStack.isEmpty) do
            f = fStack.pop()
            var gradOption = f.out.getOrElse(throw new Exception("No out")).grad
            if gradOption.isEmpty then
                gradOption = Some(DenseVector[Double](1.0))
            // println(gradOption)
            val outGrad = gradOption.get
            val inGrads = f.backward(Vector(outGrad))
            f.ins.get.zip(inGrads).foreach((in, inGrad: DenseVector[Double]) => {
                in.grad = Some(inGrad)
                if (in.creator.isDefined) then
                    fStack.push(in.creator.get)
            })
}