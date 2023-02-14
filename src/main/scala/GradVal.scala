import breeze.linalg.DenseVector
import breeze.numerics._
import java.util.{PriorityQueue, Comparator}
import breeze.linalg.support.CanZipMapValues
import algebra.ring.Semiring

case class GradVal(
    value: DenseVector[Double],
    var grad: Option[DenseVector[Double]]=Option.empty,
    var creator: Option[GradFun]= Option.empty,
    var generation: Int = 0,
    var name: Option[String] = Option.empty
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
        var f = this.creator.getOrElse(throw new Exception("No creator"))

        val queue = PriorityQueue[GradFun](new Comparator[GradFun] {
            // generationの値が大きいほど先に取り出す優先度付きキュー
            def compare(gf1: GradFun, gf2: GradFun): Int = (-1)*(gf1.generation - gf2.generation)
        })
        
        queue.add(f)
        var done = Set[GradFun]()
        while !(queue.isEmpty) do
            // 未計算の関数ノードを優先度付きキューから取り出す
            f = queue.poll()
            while done.contains(f) do
                if (queue.isEmpty) then
                    return
                else
                    f = queue.poll()
            done = done.+(f)

            // 関数ノードのbackwardメソッドを呼ぶ
            var gradOption = f.out.getOrElse(throw new Exception("No out")).grad
            if gradOption.isEmpty then
                gradOption = Some(DenseVector[Double](1.0))
                f.out.get.grad = gradOption
            val outGrad = gradOption.get
            val inGrads = f.backward(Vector(outGrad))

            // 関数ノードの入力に対する勾配を、入力変数のgradに加算する
            f.ins.get.zip(inGrads).foreach((in, inGrad: DenseVector[Double]) => {
                if in.grad.isEmpty then
                    in.grad = Some(inGrad)
                else
                    in.grad = Some(in.grad.get+inGrad)
                if (in.creator.isDefined) then
                    queue.add(in.creator.get)
            })
    
    def clearGrad(): Unit =
        this.grad = Option.empty
        // if (this.creator.isDefined) then
        //     this.creator.get.ins.get.foreach(_.clearGrad())
}