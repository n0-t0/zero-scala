// import breeze.linalg._
// import breeze.numerics._

// case class GradVal(value: DenseVector[Double], var grad: Option[DenseVector[Double]]=Option.empty, var creator: Option[GradFun]= Option.empty) {
//     def +(gv2: GradVal): GradVal = GradVal((this.value+gv2.value), Option.empty)
//     def -(gv2: GradVal): GradVal = GradVal((this.value-gv2.value), Option.empty)
//     def *(gv2: GradVal): GradVal = GradVal((this.value*gv2.value), Option.empty)
//     def /(gv2: GradVal): GradVal = GradVal((this.value-gv2.value), Option.empty)
//     def /(a: Double): GradVal = GradVal((this.value/a), Option.empty)
//     def +(a: Double): GradVal = GradVal((this.value+a), Option.empty)
//     def -(a: Double): GradVal = GradVal((this.value-a), Option.empty)
//     def *(a: Double): GradVal = GradVal((this.value*a), Option.empty)
//     def setCreator(f: GradFun): GradVal =
//         GradVal(this.value, this.grad, Some(f))
//     def backward(): Unit =
//         if (this.grad.isEmpty)
//             this.grad = Some(DenseVector[Double](1.0))
//         var f = this.creator.get
//         var fs = List[GradFun](f)
//         while !(fs.isEmpty) do
//             f = fs.last
//             fs = fs.dropRight(1)
//             val inVal = f.in.get
//             val outVal = f.out.get
//             inVal.grad = Some(f.backward(outVal.grad.get))
//             if (inVal.creator.isDefined)
//                 fs = fs.appended(inVal.creator.get)
// }

// trait F

// object F:
//     final val SQUARE: (GradVal=>GradVal) = gv => GradVal(gv.value * gv.value, Option.empty)
//     final val EXP: (GradVal=>GradVal) = gv => GradVal(exp(gv.value), Option.empty)
//     final val D_SQUARE: (GradVal=>GradVal) = gv => GradVal(gv.value * 2.0, Option.empty)
//     final val D_EXP: (GradVal=>GradVal) = gv => GradVal(exp(gv.value), Option.empty)
//     def numDiff(x: GradVal, f: (GradVal=>GradVal), h: Double=0.0001): GradVal =
//         (f(GradVal(x.value+h, Option.empty)) - f(GradVal(x.value-h, Option.empty))) / (2.0*h)
//     def zero_square(in: GradVal): GradVal = GradFun(F.SQUARE, F.D_SQUARE).forward(in)
//     def zero_exp(in: GradVal): GradVal = GradFun(F.EXP, F.D_EXP).forward(in)

// class GradFun(f: (GradVal=>GradVal), dF: (GradVal=>GradVal), var in: Option[GradVal]=Option.empty, var out: Option[GradVal]=Option.empty) extends F:
//     def forward(in: GradVal): GradVal =
//         this.in = Some(in)
//         val out = this.f(in).setCreator(this)
//         this.out = Some(out)
//         out
//     def backward(grad: DenseVector[Double]): DenseVector[Double] =
//         this.in match {
//             case Some(value) => this.dF(value).value * grad
//             case None => throw new Exception("value not found")
//         }

// // 関数ノードの作成
// val f = GradFun(F.SQUARE, F.D_SQUARE)
// // 合成関数
// val aNode = GradFun(F.SQUARE, F.D_SQUARE)
// val bNode = GradFun(F.EXP, F.D_EXP)
// val cNode = GradFun(F.SQUARE, F.D_SQUARE)
// // 順伝搬
// val input = GradVal(DenseVector(0.5))
// // val a = aNode.forward(input)
// // val b = bNode.forward(a)
// // val c = cNode.forward(b)
// val output = F.zero_square(F.zero_exp(F.zero_square(input)))
// // // 逆伝搬
// // c.grad = Some(DenseVector[Double](1.0))
// // b.grad = Some(cNode.backward(c.grad.get))
// // a.grad = Some(bNode.backward(b.grad.get))
// // val inGrad = aNode.backward(a.grad.get)
// // // 普通の微分係数計算で勾配確認
// // F.numDiff(GradVal(DenseVector(0.5)), F.SQUARE.andThen(F.EXP).andThen(F.SQUARE))
// // // 自動で逆伝搬できるようにする
// // (c.creator.get == cNode)
// // (cNode.in.get == b)
// // (b.creator.get == bNode)
// // (bNode.in.get == a)
// // (a.creator.get == aNode)
// // (aNode.in.get == input)
// // output.grad = Some(DenseVector[Double](1.0))
// output.backward()
// print(input.grad)