import breeze.linalg.DenseVector
import breeze.numerics._
import java.util.{PriorityQueue, Comparator}

case class GradVal(
    value: DenseVector[Double],
    var grad: Option[DenseVector[Double]]=Option.empty,
    var creator: Option[GradFun]= Option.empty,
    var generation: Int = 0
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
        GradVal(this.value, this.grad, Some(f), f.generation+1)
    def backward(): Unit =
        // import scala.collection.mutable.Stack
        var f = this.creator.getOrElse(throw new Exception("No creator"))

        val queue = PriorityQueue[GradFun](new Comparator[GradFun] {
            def compare(gf1: GradFun, gf2: GradFun): Int = (-1)*(gf1.generation - gf2.generation)
        })
        
        queue.add(f)
        var set = Set[GradFun]()
        // var fStack = Stack[GradFun](f)
        while !(queue.isEmpty) do

            // f = fStack.pop()
            f = queue.poll()
            while set.contains(f) do
                if (queue.isEmpty) then
                    return
                else
                    f = queue.poll()
            set = set.+(f)

            var gradOption = f.out.getOrElse(throw new Exception("No out")).grad
            if gradOption.isEmpty then
                gradOption = Some(DenseVector[Double](1.0))
                f.out.get.grad = gradOption
            val outGrad = gradOption.get
            val inGrads = f.backward(Vector(outGrad))

            f.ins.get.zip(inGrads).foreach((in, inGrad: DenseVector[Double]) => {
                if in.grad.isEmpty then
                    in.grad = Some(inGrad)
                else
                    in.grad = Some(in.grad.get+inGrad)
                if (in.creator.isDefined) then
                    // fStack.push(in.creator.get)
                    queue.add(in.creator.get)
            })
            val array = queue.toArray()
            println(array.mkString(", "))
    
    def clearGrad(): Unit =
        this.grad = Option.empty
        // if (this.creator.isDefined) then
        //     this.creator.get.ins.get.foreach(_.clearGrad())
}