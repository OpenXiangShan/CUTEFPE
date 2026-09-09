package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

// Shared fixed-width arithmetic used by the nine FPE pipeline stages.
object FpeArithmetic {
  def truncU(value: UInt, width: Int): UInt = value.pad(width)(width - 1, 0)

  def truncS(value: SInt, width: Int): SInt =
    value.pad(width).asUInt(width - 1, 0).asSInt

  def sext(value: UInt, sourceWidth: Int, targetWidth: Int): UInt =
    value(sourceWidth - 1, 0).asSInt.pad(targetWidth).asUInt

  def signedMagnitude(value: UInt, sign: Bool, width: Int): SInt = {
    val magnitude = Cat(0.U(1.W), truncU(value, width - 1)).asSInt
    truncS(Mux(sign, -magnitude, magnitude), width)
  }

  def sumS(values: Seq[SInt], width: Int): SInt = {
    values.foldLeft(0.S(width.W)) { case (sum, value) =>
      truncS(sum +& truncS(value, width), width)
    }
  }

  def maxU(values: Seq[UInt], width: Int): UInt = {
    FpeCmpTree.maximum(values.map(value => truncU(value, width)), width)
  }

  def leadingZeros(value: UInt, width: Int, resultWidth: Int): UInt = {
    Mux(value.orR, PriorityEncoder(value.asBools.reverse), width.U(resultWidth.W))
  }

  def scaleType(opcode: UInt): Bool = opcode === FpeOpcode.Nvfp4F32(3, 0)

  def scaleEnabled(opcode: UInt, flags: FpeDecodeFlags): Bool = {
    val mxFp86 = opcode === FpeOpcode.Mxfp8e4m3F32(3, 0) ||
      opcode === FpeOpcode.Mxfp8e5m2F32(3, 0) ||
      opcode === FpeOpcode.Mxfp6e3m2F32(3, 0) ||
      opcode === FpeOpcode.Mxfp6e2m3F32(3, 0)
    mxFp86 || flags.mxIntFp4
  }

  def bias(opcode: UInt): SInt = {
    MuxCase((-127).S(8.W), Seq(
      ((opcode === FpeOpcode.TF32TF32F32(3, 0)) ||
        (opcode === FpeOpcode.BF16BF16F32(3, 0))) -> 127.S(8.W),
      ((opcode === FpeOpcode.F16F16F32(3, 0)) ||
        (opcode === FpeOpcode.Mxfp8e5m2F32(3, 0)) ||
        (opcode === FpeOpcode.Fp8e5m2F32(3, 0))) -> (-97).S(8.W),
      ((opcode === FpeOpcode.Mxfp8e4m3F32(3, 0)) ||
        (opcode === FpeOpcode.Fp8e4m3F32(3, 0))) -> (-113).S(8.W),
      (opcode === FpeOpcode.Mxfp6e3m2F32(3, 0)) -> (-121).S(8.W),
      (opcode === FpeOpcode.Mxfp6e2m3F32(3, 0)) -> (-125).S(8.W)
    ))
  }

  def scaleExp(byte: UInt, scaleType: Bool, scaleEn: Bool): UInt = {
    val nzE4 = byte(6, 3).orR
    val zero = !byte(6, 0).orR
    Mux(!scaleEn, 127.U(8.W),
      Mux(!scaleType, byte,
        Mux(nzE4 || zero, Cat(0.U(4.W), byte(6, 3)), 1.U(8.W))))
  }

  def scaleSig(byte: UInt, scaleType: Bool): UInt =
    Mux(!scaleType, 8.U(4.W), Cat(byte(6, 3).orR, byte(2, 0)))

  def scaleZero(byte: UInt, scaleType: Bool): Bool = scaleType && !byte(6, 0).orR

  def scaleNan(bits: UInt, scaleType: Bool, scaleEn: Bool, scaleNum: Int): Bool = {
    val nan = (0 until scaleNum).map { index =>
      val byte = bits(index * 8 + 7, index * 8)
      Mux(scaleType, byte(6, 0).andR, byte.andR)
    }.reduce(_ || _)
    scaleEn && nan
  }

  private def mulLow(a: UInt, b: UInt, width: Int): UInt = truncU(a * b, width)

  /** Mirrors decode_sig + sig_mul + product_sum for one 32-bit lane. */
  def mulLane(
      a8: Vec[UInt],
      a4: Vec[UInt],
      b4: Vec[UInt],
      b8: Vec[UInt],
      expE6: Vec[UInt],
      sign: UInt,
      flags: FpeDecodeFlags
  ): FpeMulResult = {
    val out = Wire(new FpeMulResult)
    out := 0.U.asTypeOf(out)

    val a84 = Wire(Vec(2, UInt(8.W)))
    val a88 = Wire(Vec(2, UInt(8.W)))
    val a48 = Wire(Vec(2, UInt(4.W)))
    val a44 = Wire(Vec(2, UInt(4.W)))
    val b84 = Wire(Vec(2, UInt(8.W)))
    val b88 = Wire(Vec(2, UInt(8.W)))
    val b48 = Wire(Vec(2, UInt(8.W)))
    val b44 = Wire(Vec(2, UInt(4.W)))

    for (i <- 0 until 2) {
      val highA84 = a8(i)
      val highA88 = a8(i)
      val highA48 = a4(i)
      val highA44 = a4(i)
      val highB84 = b4(i).pad(8)
      val highB88 = b8(i)
      val highB48 = b8(i)
      val highB44 = b4(i)

      val lowA84 = 0.U(8.W)
      val lowA88 = a4(i).pad(8)
      val lowA48 = a8(i)(3, 0)
      val lowA44 = 0.U(4.W)
      val lowB84 = 0.U(8.W)
      val lowB88 = b4(i).pad(8)
      val lowB48 = b8(i)(3, 0).pad(8)
      val lowB44 = 0.U(4.W)

      val intA84 = a8(i)(3, 0).pad(8)
      val intA88 = a4(i).pad(8)
      val intA48 = a8(i)(7, 4)
      val intA44 = expE6(i)(3, 0)
      val intB84 = b8(i)(3, 0).pad(8)
      val intB88 = b4(i).pad(8)
      val intB48 = b8(i)(7, 4).pad(8)
      val intB44 = expE6(i + 2)(3, 0)

      a84(i) := Mux(flags.highPrecision, highA84,
        Mux(flags.lowPrecision, lowA84, Mux(flags.intFp4, intA84, a8(i))))
      a88(i) := Mux(flags.highPrecision, highA88,
        Mux(flags.lowPrecision, lowA88, Mux(flags.intFp4, intA88,
          Cat(a4(i), expE6(i)(3, 0)))))
      a48(i) := Mux(flags.highPrecision, highA48,
        Mux(flags.lowPrecision, lowA48, Mux(flags.intFp4, intA48, b8(i)(3, 0))))
      a44(i) := Mux(flags.highPrecision, highA44,
        Mux(flags.lowPrecision, lowA44, Mux(flags.intFp4, intA44, 0.U(4.W))))
      b84(i) := Mux(flags.highPrecision, highB84,
        Mux(flags.lowPrecision, lowB84, Mux(flags.intFp4, intB84, b8(i)(7, 4).pad(8))))
      b88(i) := Mux(flags.highPrecision, highB88,
        Mux(flags.lowPrecision, lowB88, Mux(flags.intFp4, intB88,
          Cat(b4(i), expE6(i + 2)(3, 0)))))
      b48(i) := Mux(flags.highPrecision, highB48,
        Mux(flags.lowPrecision, lowB48, Mux(flags.intFp4, intB48, a8(i))))
      b44(i) := Mux(flags.highPrecision, highB44,
        Mux(flags.lowPrecision, lowB44, Mux(flags.intFp4, intB44, 0.U(4.W))))
    }

    val product00 = mulLow(a84(0), b84(0), 12)
    val product01 = mulLow(a48(0), b48(0), 12)
    val product10 = mulLow(a88(0), b88(0), 16)
    val product11 = mulLow(a44(0), b44(0), 8)
    val product20 = mulLow(a84(1), b84(1), 12)
    val product21 = mulLow(a48(1), b48(1), 12)
    val product30 = mulLow(a88(1), b88(1), 16)
    val product31 = mulLow(a44(1), b44(1), 8)

    def intFpProduct(product: UInt, signBit: Bool): SInt =
      signedMagnitude(product(7, 0), signBit, 9)

    val int00 = intFpProduct(product00, sign(0))
    val int01 = intFpProduct(product01, sign(1))
    val int10 = intFpProduct(product10, sign(2))
    val int11 = intFpProduct(product11, sign(3))
    val int20 = intFpProduct(product20, sign(4))
    val int21 = intFpProduct(product21, sign(5))
    val int30 = intFpProduct(product30, sign(6))
    val int31 = intFpProduct(product31, sign(7))

    def packProduct(
        p00: UInt,
        p01: UInt,
        p10: UInt,
        p11: UInt,
        i00: SInt,
        i01: SInt,
        i10: SInt,
        i11: SInt,
        exp0: UInt,
        exp1: UInt
    ): (UInt, UInt, UInt, UInt) = {
      val sam00 = Mux(flags.intFp4, sext(i00.asUInt, 9, 12), p00)
      val sam01 = Mux(flags.intFp4, sext(i01.asUInt, 9, 11),
        Mux(flags.int8, Cat(0.U(3.W), p01(11, 4)), p01(10, 0)))
      val sam0 = truncU(sam00 + sam01, 12)
      val samL0 = Mux(flags.int8, Cat(sam0(6, 0), p01(3, 0)), sam0(10, 0))
      val expOut0 = Mux(flags.int8, Cat(0.U(1.W), sam0(11, 7)), exp0)

      val samHSource = Cat(p10, p11)
      val int1 = truncS(i10 +& i11, 10)
      val samL1 = Mux(flags.intFp4, sext(int1.asUInt, 10, 11), p10(10, 0))
      val expOut1 = Mux(flags.int8, Cat(0.U(1.W), p10(15, 11)), exp1)
      val samH = Cat(truncU(sam0 + samHSource(23, 4), 20), samHSource(3, 0))
      val packed = Mux(flags.highPrecision, samH(21, 0), Cat(samL1, samL0))
      (packed, expOut0, expOut1, samH)
    }

    val first = packProduct(product00, product01, product10, product11,
      int00, int01, int10, int11, expE6(0), expE6(1))
    val second = packProduct(product20, product21, product30, product31,
      int20, int21, int30, int31, expE6(2), expE6(3))
    out.product(0) := first._1
    out.product(1) := second._1
    out.expE6(0) := first._2
    out.expE6(1) := first._3
    out.expE6(2) := second._2
    out.expE6(3) := second._3
    out
  }
}

/** Decode and capture the transaction payload that crosses pipe0. */
class FReduceMACPipe0(implicit p: Parameters) extends CuteFpeModule {
  private val laneNum = ReduceWidth / 32
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize
  private val mxFp86GroupSize = if (laneNum >= 8) 8 else laneNum
  private val mxFp86GroupNum = laneNum / mxFp86GroupSize
  private val scaleBusWidth = scaleNum * ScaleElementWidth

  val io = IO(new Bundle {
    val aVector = Input(UInt(ReduceWidth.W))
    val bVector = Input(UInt(ReduceWidth.W))
    val cAdd = Input(UInt(ResultWidth.W))
    val aScale = Input(UInt(scaleBusWidth.W))
    val bScale = Input(UInt(scaleBusWidth.W))
    val opcode = Input(UInt(FpeOpcode.Width.W))
    val out = Output(new FPipe0Result)
  })

  val flags = FVecDecoder.flags(io.opcode)
  val decoded = (0 until laneNum).map { lane =>
    FVecDecoder.decodeLane(
      io.aVector(lane * 32 + 31, lane * 32),
      io.bVector(lane * 32 + 31, lane * 32),
      io.opcode
    )
  }

  io.out := 0.U.asTypeOf(io.out)
  io.out.cAdd := io.cAdd
  io.out.aScale := io.aScale
  io.out.bScale := io.bScale
  io.out.opcode := Mux(flags.int4 || flags.mxInt4, 15.U(4.W), io.opcode(3, 0))
  io.out.flags := flags
  // decodeLane performs the four-element exception reduction within one
  // 32-bit lane.  The top-level decode then applies the same reduction over
  // lanes: a +Inf and a -Inf from different lanes form NaN rather than two
  // independent infinity flags.
  val anyNan = decoded.map(_.nan).reduce(_ || _)
  val anyPInf = decoded.map(_.pInf).reduce(_ || _)
  val anyNInf = decoded.map(_.nInf).reduce(_ || _)
  io.out.nan := anyNan || (anyPInf && anyNInf)
  io.out.pInf := !io.out.nan && anyPInf
  io.out.nInf := !io.out.nan && anyNInf

  for (lane <- 0 until laneNum) {
    io.out.a8(lane) := decoded(lane).a8
    io.out.a4(lane) := decoded(lane).a4
    io.out.b4(lane) := decoded(lane).b4
    io.out.b8(lane) := decoded(lane).b8
    io.out.expE6(lane) := decoded(lane).expE6
    io.out.vec8Sign(lane) := decoded(lane).sign
  }

  for (index <- 0 until scaleNum) {
    io.out.mxIntFp4AllZero(index) :=
      decoded(index * 2).mxIntFp4AllZero && decoded(index * 2 + 1).mxIntFp4AllZero
  }

  for (group <- 0 until mxFp86GroupNum) {
    val allZero = decoded.slice(group * mxFp86GroupSize,
      (group + 1) * mxFp86GroupSize).map(_.mxFp86AllZero).reduce(_ && _)
    io.out.mxFp86AllZero(group) := FpeOpcode.isMxfp8OrFp6(io.opcode) && allZero
  }
}

/** Scale decode, mantissa multiply, and exponent preparation for pipe1. */
class FReduceMACPipe1(implicit p: Parameters) extends CuteFpeModule {
  private val laneNum = ReduceWidth / 32
  private val blockNum = laneNum / 2
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize
  private val mxFp86GroupSize = if (laneNum >= 8) 8 else laneNum
  private val mxFp86GroupNum = laneNum / mxFp86GroupSize

  val io = IO(new Bundle {
    val in = Input(new FPipe0Result)
    val out = Output(new FPipe1Result)
  })

  val scaleType = FpeArithmetic.scaleType(io.in.opcode)
  val scaleEn = FpeArithmetic.scaleEnabled(io.in.opcode, io.in.flags)
  val rawExp = Wire(Vec(scaleNum, UInt(9.W)))
  val rawSig = Wire(Vec(scaleNum, UInt(8.W)))

  io.out := 0.U.asTypeOf(io.out)

  // This is the first half of the exponent comparison tree.  Pipe2 receives
  // only the selected candidates and the remaining three exponent bits.
  for (block <- 0 until 2) {
    val laneBase = block * blockNum
    val e3Input0 = (0 until blockNum).map { lane => io.in.expE6(laneBase + lane)(0) }
    val e3Input1 = (0 until blockNum).map { lane => io.in.expE6(laneBase + lane)(1) }
    val e6Input0 = (0 until blockNum).map { lane => io.in.expE6(laneBase + lane)(2) }
    val e6Input1 = (0 until blockNum).map { lane => io.in.expE6(laneBase + lane)(3) }
    val e3Partial0 = FpeCmpTree.prefix(e3Input0, 6, 3)
    val e3Partial1 = FpeCmpTree.prefix(e3Input1, 6, 3)
    val e6Mask0 = FpeCmpTree.mask(e6Input0, 6)
    val e6Mask1 = FpeCmpTree.mask(e6Input1, 6)

    io.out.candidateMask(block)(0) := Mux(io.in.flags.highPrecision, e6Mask0, e3Partial0.mask)
    io.out.candidateMask(block)(1) := Mux(io.in.flags.highPrecision, e6Mask1, e3Partial1.mask)
    io.out.remainingE3(block)(0) := e3Partial0.remaining
    io.out.remainingE3(block)(1) := e3Partial1.remaining
    io.out.e3Max(block)(0) := e3Partial0.maximum
    io.out.e3Max(block)(1) := e3Partial1.maximum
    io.out.e6Max(block)(0) := FpeCmpTree.maximum(e6Input0, 6)
    io.out.e6Max(block)(1) := FpeCmpTree.maximum(e6Input1, 6)
  }

  for (index <- 0 until scaleNum) {
    val aByte = io.in.aScale(index * ScaleElementWidth + 7, index * ScaleElementWidth)
    val bByte = io.in.bScale(index * ScaleElementWidth + 7, index * ScaleElementWidth)
    val aExp = FpeArithmetic.scaleExp(aByte, scaleType, scaleEn)
    val bExp = FpeArithmetic.scaleExp(bByte, scaleType, scaleEn)
    val aSig = FpeArithmetic.scaleSig(aByte, scaleType)
    val bSig = FpeArithmetic.scaleSig(bByte, scaleType)
    val zeroByScale = io.in.flags.mxIntFp4 &&
      (FpeArithmetic.scaleZero(aByte, scaleType) ||
        FpeArithmetic.scaleZero(bByte, scaleType) || io.in.mxIntFp4AllZero(index))
    val zeroByMxFp86 = if (index < mxFp86GroupNum) io.in.mxFp86AllZero(index) else false.B
    rawExp(index) := Mux(zeroByScale || zeroByMxFp86, 0.U(9.W),
      FpeArithmetic.truncU(aExp.pad(9) + bExp.pad(9), 9))
    rawSig(index) := FpeArithmetic.truncU(aSig * bSig, 8)
    io.out.scaleExp(index) := Mux(scaleType, rawExp(index), rawExp(index / 2))
    io.out.scaleSig(index) := Mux(scaleType, rawSig(index), rawSig(index / 2))
  }

  val scaleNan = FpeArithmetic.scaleNan(io.in.aScale, scaleType, scaleEn, scaleNum) ||
    FpeArithmetic.scaleNan(io.in.bScale, scaleType, scaleEn, scaleNum)
  io.out.cAdd := io.in.cAdd
  io.out.nan := io.in.nan || scaleNan
  io.out.pInf := io.in.pInf && !scaleNan
  io.out.nInf := io.in.nInf && !scaleNan
  io.out.flags := io.in.flags
  io.out.scaleType := scaleType
  io.out.thresholdNormal := io.in.opcode === FpeOpcode.TF32TF32F32(3, 0) ||
    io.in.opcode === FpeOpcode.BF16BF16F32(3, 0) || FpeOpcode.isMxfp8OrFp6(io.in.opcode)
  io.out.thresholdMxIntFp4 := io.in.flags.mxIntFp4 && !scaleType
  io.out.bias := FpeArithmetic.bias(io.in.opcode)

  for (lane <- 0 until laneNum) {
    val multiply = FpeArithmetic.mulLane(io.in.a8(lane), io.in.a4(lane), io.in.b4(lane),
      io.in.b8(lane), io.in.expE6(lane), io.in.vec8Sign(lane), io.in.flags)
    io.out.product(lane) := multiply.product
    io.out.expE6(lane) := multiply.expE6
    io.out.vec4Sign(lane) := io.in.vec8Sign(lane)(3, 0)
  }
}

/** Decode C and form the two partial exponent domains for pipe2. */
class FReduceMACPipe2(implicit p: Parameters) extends CuteFpeModule {
  private val laneNum = ReduceWidth / 32
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize

  val io = IO(new Bundle {
    val in = Input(new FPipe1Result)
    val out = Output(new FPipe2Result)
  })

  private def eabMax(block: Int): UInt = {
    val e3Later0 = FpeCmpTree.finish(
      io.in.candidateMask(block)(0), io.in.remainingE3(block)(0).toSeq)
    val e3Later1 = FpeCmpTree.finish(
      io.in.candidateMask(block)(1), io.in.remainingE3(block)(1).toSeq)
    val e6_01 = Mux(io.in.e6Max(block)(0) > io.in.e6Max(block)(1),
      io.in.e6Max(block)(0), io.in.e6Max(block)(1))
    val e6_23_0 = Cat(io.in.e3Max(block)(0), e3Later0)
    val e6_23_1 = Cat(io.in.e3Max(block)(1), e3Later1)
    val e6_23 = Mux(e6_23_0 > e6_23_1, e6_23_0, e6_23_1)
    val e9_0 = Cat(io.in.e6Max(block)(0), e3Later0)
    val e9_1 = Cat(io.in.e6Max(block)(1), e3Later1)
    val e9 = Mux(e9_0 > e9_1, e9_0, e9_1)

    Mux(io.in.flags.highPrecision, e9,
      Mux(e6_23 > e6_01, Cat(0.U(3.W), e6_23), Cat(0.U(3.W), e6_01)))
  }

  val cSign = io.in.cAdd(31)
  val cExpField = io.in.cAdd(30, 23)
  val cFraction = io.in.cAdd(22, 0)
  val cHasExponent = cExpField.orR
  val cZero = !(cHasExponent || cFraction.orR)
  val cNan = cExpField.andR && cFraction.orR
  val cPInf = !cSign && cExpField.andR && !cFraction.orR
  val cNInf = cSign && cExpField.andR && !cFraction.orR
  val intC = io.in.flags.int8 || io.in.flags.int4
  val cSig = Mux(intC, io.in.cAdd,
    Mux(cHasExponent, Cat(0.U(8.W), 1.U(1.W), cFraction),
      Cat(0.U(8.W), 0.U(1.W), cFraction)))
  val thresholdBase = 248.S(10.W)
  val scaleIndex1 = if (scaleNum == 8) 2 else 1
  val threshold0 = FpeArithmetic.truncS(
    thresholdBase +& io.in.bias.pad(10) - io.in.scaleExp(0).pad(10).asSInt, 10)
  val threshold1 = FpeArithmetic.truncS(
    thresholdBase +& io.in.bias.pad(10) - io.in.scaleExp(scaleIndex1).pad(10).asSInt, 10)

  io.out := 0.U.asTypeOf(io.out)
  io.out.cExp := Mux(cHasExponent, cExpField, 1.U(8.W))
  io.out.cSig := cSig
  io.out.cSign := cSign
  io.out.cZero := cZero
  io.out.eabMax0 := eabMax(0)
  io.out.eabMax1 := eabMax(1)
  io.out.scaleMax := FpeArithmetic.maxU((0 until scaleNum).map(io.in.scaleExp(_)), 9)
  io.out.thresholdMxIntFp4Enable := io.in.thresholdMxIntFp4 && cZero
  io.out.thresholdNormalExp0 := Mux(io.in.thresholdNormal && cZero, threshold0.asUInt, 0.U(10.W))
  io.out.thresholdNormalExp1 := Mux(io.in.thresholdNormal && cZero, threshold1.asUInt, 0.U(10.W))
  io.out.scaleExpS0 := FpeArithmetic.truncS(io.in.scaleExp(0).asSInt - 254.S(9.W), 9)
  io.out.scaleExpS1 := FpeArithmetic.truncS(io.in.scaleExp(scaleIndex1).asSInt - 254.S(9.W), 9)
  io.out.bias := io.in.bias
  io.out.nan := (io.in.nan || cNan) || (cNInf && io.in.pInf) || (cPInf && io.in.nInf)
  io.out.pInf := !(io.in.nan || cNan) && ((io.in.pInf && !cNInf) || (!io.in.nInf && cPInf))
  io.out.nInf := !(io.in.nan || cNan) && ((!io.in.pInf && cNInf) || (io.in.nInf && !cPInf))
  io.out.flags := io.in.flags
  io.out.scaleType := io.in.scaleType
  io.out.scaleExp := io.in.scaleExp
  io.out.scaleSig := io.in.scaleSig
  io.out.product := io.in.product
  io.out.expE6 := io.in.expE6
  io.out.vec4Sign := io.in.vec4Sign
}

/** Form shift plans for the two exponent blocks in pipe3. */
class FReduceMACPipe3(implicit p: Parameters) extends CuteFpeModule {
  private val laneNum = ReduceWidth / 32
  private val blockNum = laneNum / 2
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize

  val io = IO(new Bundle {
    val in = Input(new FPipe2Result)
    val out = Output(new FPipe3Result)
  })

  val thresholdMx = io.in.thresholdMxIntFp4Enable && io.in.scaleMax < 115.U(9.W)
  val scaleBias = Mux(io.in.scaleType, 7.S(10.W), 127.S(10.W))
  val twiceScaleBias = FpeArithmetic.truncS(scaleBias << 1, 10)
  val cExpTemp = Mux(io.in.cZero, "h200".U(10.W).asSInt,
    FpeArithmetic.truncS(io.in.cExp.pad(10).asSInt - 127.S(10.W) + twiceScaleBias, 10))
  val scaleMaxTemp = FpeArithmetic.truncS(
    io.in.scaleMax.pad(10).asSInt - twiceScaleBias + 127.S(10.W), 10)
  val cDominatesScale = cExpTemp > io.in.scaleMax.pad(10).asSInt || !io.in.scaleMax.orR
  val blockMax = Mux(thresholdMx, (-12).S(10.W),
    Mux(cDominatesScale, io.in.cExp.pad(10).asSInt, scaleMaxTemp))
  val shiftTemp = FpeArithmetic.truncS(Mux(thresholdMx, 115.S(9.W),
    Mux(cDominatesScale, cExpTemp, io.in.scaleMax.asSInt)), 9)

  val thresholdNormal = io.in.thresholdNormalExp0.asSInt > io.in.eabMax0.pad(10).asSInt &&
    io.in.thresholdNormalExp1.asSInt > io.in.eabMax1.pad(10).asSInt
  val eabMaxS0 = FpeArithmetic.truncS(
    io.in.eabMax0.pad(10).asSInt +& io.in.scaleExpS0.pad(10), 10)
  val eabMaxS1 = FpeArithmetic.truncS(
    io.in.eabMax1.pad(10).asSInt +& io.in.scaleExpS1.pad(10), 10)
  val eabMaxS = Mux(eabMaxS0 > eabMaxS1, eabMaxS0, eabMaxS1)
  val cTemp = Mux(io.in.cZero, "h200".U(10.W).asSInt,
    FpeArithmetic.truncS(io.in.cExp.pad(10).asSInt +& io.in.bias.pad(10), 10))
  val eMaxShift = Mux(cTemp < eabMaxS || thresholdNormal, eabMaxS, cTemp)
  val eMaxShift0 = Mux(thresholdNormal, io.in.thresholdNormalExp0,
    FpeArithmetic.truncS(eMaxShift - io.in.scaleExpS0.pad(10), 10).asUInt)
  val eMaxShift1 = Mux(thresholdNormal, io.in.thresholdNormalExp1,
    FpeArithmetic.truncS(eMaxShift - io.in.scaleExpS1.pad(10), 10).asUInt)
  val cShiftRaw = FpeArithmetic.truncU(eabMaxS.asUInt - cTemp.asUInt, 10)

  io.out := 0.U.asTypeOf(io.out)
  io.out.blockMax := blockMax
  io.out.nvcShift := {
    val raw = FpeArithmetic.truncU(shiftTemp.asUInt - cExpTemp.asUInt, 9)
    Mux(raw(8, 6).orR, 63.U(6.W), raw(5, 0))
  }
  io.out.eMax := Mux(thresholdNormal, (-6).S(10.W),
    FpeArithmetic.truncS(eMaxShift - io.in.bias.pad(10), 10))
  io.out.cShift := Mux(cTemp > eabMaxS, 0.U(5.W),
    Mux(cShiftRaw(9, 5).orR, 31.U(5.W), cShiftRaw(4, 0)))

  for (index <- 0 until scaleNum) {
    val raw = FpeArithmetic.truncU(shiftTemp.asUInt - io.in.scaleExp(index), 9)
    io.out.blockShift(index) := Mux(raw(8, 6).orR, 63.U(6.W), raw(5, 0))
  }

  for (lane <- 0 until laneNum) {
    val laneEMaxShift = if (lane < blockNum) eMaxShift0 else eMaxShift1
    val e9_0 = Cat(io.in.expE6(lane)(2), io.in.expE6(lane)(0)(2, 0))
    val e9_1 = Cat(io.in.expE6(lane)(3), io.in.expE6(lane)(1)(2, 0))
    val hpDiff0 = FpeArithmetic.truncU(laneEMaxShift(8, 0) - e9_0, 9)
    val hpDiff1 = FpeArithmetic.truncU(laneEMaxShift(8, 0) - e9_1, 9)
    val lpDiff = (0 until 4).map { index =>
      FpeArithmetic.truncU(laneEMaxShift(6, 0) - Cat(0.U(1.W), io.in.expE6(lane)(index)), 7)
    }
    val hpShift0 = Mux(hpDiff0(8, 5).orR, 31.U(5.W), hpDiff0(4, 0))
    val hpShift1 = Mux(hpDiff1(8, 5).orR, 31.U(5.W), hpDiff1(4, 0))
    val lpShift = lpDiff.map { difference =>
      Mux(laneEMaxShift(9, 7).orR || difference(6, 5).orR, 31.U(5.W), difference(4, 0))
    }
    io.out.expShift(lane)(0) := Mux(io.in.flags.int8, io.in.expE6(lane)(0)(4, 0),
      Mux(io.in.flags.highPrecision, hpShift0,
        Mux(io.in.flags.lowPrecision, lpShift(0), 0.U(5.W))))
    io.out.expShift(lane)(1) := Mux(io.in.flags.int8, io.in.expE6(lane)(1)(4, 0),
      Mux(io.in.flags.highPrecision, hpShift1,
        Mux(io.in.flags.lowPrecision, lpShift(1), 0.U(5.W))))
    io.out.expShift(lane)(2) := Mux(io.in.flags.int8, io.in.expE6(lane)(2)(4, 0),
      Mux(io.in.flags.lowPrecision, lpShift(2), 0.U(5.W)))
    io.out.expShift(lane)(3) := Mux(io.in.flags.int8, io.in.expE6(lane)(3)(4, 0),
      Mux(io.in.flags.lowPrecision, lpShift(3), 0.U(5.W)))
  }

  io.out.nan := io.in.nan
  io.out.pInf := io.in.pInf
  io.out.nInf := io.in.nInf
  io.out.cSig := io.in.cSig
  io.out.cSign := io.in.cSign
  io.out.scaleSig := io.in.scaleSig
  io.out.product := io.in.product
  io.out.vec4Sign := io.in.vec4Sign
  io.out.flags := io.in.flags
}

/** Shift and reduce lane products through the first addition tree. */
class FReduceMACPipe4(implicit p: Parameters) extends CuteFpeModule {
  private val laneNum = ReduceWidth / 32
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val add0Width = 28 + log2Ceil(4) + log2Ceil(2)

  val io = IO(new Bundle {
    val in = Input(new FPipe3Result)
    val out = Output(new FPipe4Result)
  })

  val laneSums = (0 until laneNum).map { lane =>
    val shifted = Wire(Vec(4, UInt(27.W)))
    shifted(0) := Mux(io.in.flags.highPrecision,
      FpeArithmetic.truncU(Cat(io.in.product(lane)(0), 0.U(5.W)) >> io.in.expShift(lane)(0), 27),
      Mux(io.in.flags.lowPrecision,
        FpeArithmetic.truncU(Cat(io.in.product(lane)(0)(9, 0), 0.U(19.W)) >> io.in.expShift(lane)(0), 27),
        Cat(0.U(11.W), io.in.expShift(lane)(0), io.in.product(lane)(0)(10, 0))))
    shifted(1) := Mux(io.in.flags.highPrecision,
      FpeArithmetic.truncU(Cat(io.in.product(lane)(1), 0.U(5.W)) >> io.in.expShift(lane)(1), 27),
      Mux(io.in.flags.lowPrecision,
        FpeArithmetic.truncU(Cat(io.in.product(lane)(0)(20, 11), 0.U(19.W)) >> io.in.expShift(lane)(1), 27),
        Cat(0.U(11.W), io.in.expShift(lane)(1), io.in.product(lane)(0)(21, 11))))
    shifted(2) := Mux(io.in.flags.highPrecision, 0.U(27.W),
      Mux(io.in.flags.lowPrecision,
        FpeArithmetic.truncU(Cat(io.in.product(lane)(1)(9, 0), 0.U(19.W)) >> io.in.expShift(lane)(2), 27),
        Cat(0.U(11.W), io.in.expShift(lane)(2), io.in.product(lane)(1)(10, 0))))
    shifted(3) := Mux(io.in.flags.highPrecision, 0.U(27.W),
      Mux(io.in.flags.lowPrecision,
        FpeArithmetic.truncU(Cat(io.in.product(lane)(1)(20, 11), 0.U(19.W)) >> io.in.expShift(lane)(3), 27),
        Cat(0.U(11.W), io.in.expShift(lane)(3), io.in.product(lane)(1)(21, 11))))
    val signed = (0 until 4).map { index =>
      val mxValue = FpeArithmetic.sext(shifted(index)(9, 0), 10, 28).asSInt
      val magnitude = Cat(0.U(1.W), shifted(index)).asSInt
      val signedMagnitude = FpeArithmetic.truncS(
        Mux(io.in.vec4Sign(lane)(index), -magnitude, magnitude), 28)
      // Pipe3 in the SV design extends this control to ordinary INT4 before
      // entering sign_shift: product_sum has already packed signed 11-bit
      // partial sums for every INT/FP4 format.  The Scale control remains
      // limited to MX/NV and MXINT4 elsewhere; only this sign extension must
      // include the non-Scale INT4 opcodes.
      Mux(io.in.flags.int4 || io.in.flags.mxIntFp4, mxValue, signedMagnitude)
    }
    FpeArithmetic.sumS(signed, 30)
  }

  io.out := 0.U.asTypeOf(io.out)
  for (index <- 0 until p4Add0OutNum) {
    io.out.signAfterAdd0(index) := FpeArithmetic.sumS(
      laneSums.slice(index * 2, index * 2 + 2), add0Width)
  }
  io.out.blockMax := io.in.blockMax
  io.out.blockShift := io.in.blockShift
  io.out.nvcShift := io.in.nvcShift
  io.out.eMax := io.in.eMax
  io.out.cShift := io.in.cShift
  io.out.nan := io.in.nan
  io.out.pInf := io.in.pInf
  io.out.nInf := io.in.nInf
  io.out.cSig := io.in.cSig
  io.out.cSign := io.in.cSign
  io.out.scaleSig := io.in.scaleSig
  io.out.mxNvFp4 := io.in.flags.mxNvFp4
  io.out.int8 := io.in.flags.int8
  io.out.int4 := io.in.flags.int4
  io.out.mxInt4 := io.in.flags.mxInt4
}

/** Complete the second addition tree and the block-scale product path. */
class FReduceMACPipe5(implicit p: Parameters) extends CuteFpeModule {
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val addNum2 = 4 / (512 / ReduceWidth)
  private val add1Width = 28 + log2Ceil(4) + log2Ceil(2) + log2Ceil(addNum2)
  private val blockSigWidth = 35 + 10

  val io = IO(new Bundle {
    val in = Input(new FPipe4Result)
    val out = Output(new FPipe5Result)
  })

  io.out := 0.U.asTypeOf(io.out)
  for (index <- 0 until 2) {
    io.out.signAfterAdd1(index) := FpeArithmetic.sumS(
      io.in.signAfterAdd0.slice(index * addNum2, index * addNum2 + addNum2), add1Width)
  }
  for (index <- 0 until p4Add0OutNum) {
    val low13 = io.in.signAfterAdd0(index)(12, 0).asSInt
    val negative = low13 < 0.S
    val magnitude = FpeArithmetic.truncU(Mux(negative, (-low13).asUInt, low13.asUInt), 13)
    val scaleProduct = FpeArithmetic.truncU(io.in.scaleSig(index) * magnitude, 21)
    val mxIntBlockSig = Cat(0.U(1.W), magnitude, 0.U(31.W))
    // The block-scale path forms a 48-bit product alignment and retains the
    // low 45 bits at this boundary, matching the fixed-width SV assignment.
    val fp4BlockSig = FpeArithmetic.truncU(Cat(scaleProduct, 0.U(27.W)), blockSigWidth)
    val blockSig = Mux(io.in.mxInt4, mxIntBlockSig, fp4BlockSig)
    val shifted = FpeArithmetic.truncU(blockSig(43, 0) >> io.in.blockShift(index), 44)
    val signed = Cat(0.U(1.W), shifted).asSInt
    io.out.blockSigAfterShift(index) := FpeArithmetic.truncS(
      Mux(negative, -signed, signed), blockSigWidth)
  }
  io.out.blockMax := io.in.blockMax
  io.out.nvcShift := io.in.nvcShift
  io.out.eMax := io.in.eMax
  io.out.cShift := io.in.cShift
  io.out.nan := io.in.nan
  io.out.pInf := io.in.pInf
  io.out.nInf := io.in.nInf
  io.out.cSig := io.in.cSig
  io.out.cSign := io.in.cSign
  io.out.mxIntFp4 := io.in.mxNvFp4 || io.in.mxInt4
  io.out.intResult := io.in.int8 || io.in.int4
}

/** Add C and finish both reduction paths in pipe6. */
class FReduceMACPipe6(implicit p: Parameters) extends CuteFpeModule {
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val addNum2 = 4 / (512 / ReduceWidth)
  private val fp8Width = 28 + log2Ceil(4) + log2Ceil(2) + log2Ceil(addNum2) + 1
  private val nvSigWidth = 35 + 10 + log2Ceil(p4Add0OutNum) + 1

  val io = IO(new Bundle {
    val in = Input(new FPipe5Result)
    val out = Output(new FPipe6Result)
  })

  val fp8CUnsigned = FpeArithmetic.truncU(Cat(io.in.cSig(23, 0), 0.U(2.W)) >> io.in.cShift, 26)
  val nvCUnsigned = FpeArithmetic.truncU(Cat(io.in.cSig(23, 0), 0.U(12.W)) >> io.in.nvcShift, 36)
  val fp8CSig = FpeArithmetic.truncS(Mux(io.in.cSign,
    -Cat(0.U(1.W), fp8CUnsigned).asSInt,
    Cat(0.U(1.W), fp8CUnsigned).asSInt), 27)
  val nvCSig = FpeArithmetic.truncS(Mux(io.in.cSign,
    -Cat(0.U(1.W), nvCUnsigned).asSInt,
    Cat(0.U(1.W), nvCUnsigned).asSInt), 37)
  val fp8Sum = FpeArithmetic.sumS(io.in.signAfterAdd1.toSeq, fp8Width)
  val intSum = FpeArithmetic.sumS(io.in.signAfterAdd1.toSeq :+ io.in.cSig.asSInt, fp8Width)
  val fpSum = FpeArithmetic.truncS(fp8Sum +& fp8CSig, fp8Width)
  val blockSum = FpeArithmetic.sumS(io.in.blockSigAfterShift.toSeq, nvSigWidth)

  io.out := 0.U.asTypeOf(io.out)
  io.out.signFp8 := Mux(io.in.intResult, intSum, fpSum)
  io.out.nvMxFp4Sig := FpeArithmetic.truncS(blockSum +& nvCSig, nvSigWidth)
  io.out.eMax := io.in.eMax
  io.out.blockMax := io.in.blockMax
  io.out.nan := io.in.nan
  io.out.pInf := io.in.pInf
  io.out.nInf := io.in.nInf
  io.out.mxIntFp4 := io.in.mxIntFp4
  io.out.intResult := io.in.intResult
}

/** Normalize the selected reduction result into FP32 fields in pipe7. */
class FReduceMACPipe7(implicit p: Parameters) extends CuteFpeModule {
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val addNum2 = 4 / (512 / ReduceWidth)
  private val fp8Width = 28 + log2Ceil(4) + log2Ceil(2) + log2Ceil(addNum2) + 1
  private val nvSigWidth = 35 + 10 + log2Ceil(p4Add0OutNum) + 1

  val io = IO(new Bundle {
    val in = Input(new FPipe6Result)
    val out = Output(new FPipe7Result)
  })

  val fp8Magnitude = FpeArithmetic.truncU(Mux(io.in.signFp8 < 0.S,
    (-io.in.signFp8).asUInt, io.in.signFp8.asUInt), fp8Width - 1)
  val nvMagnitude = FpeArithmetic.truncU(Mux(io.in.nvMxFp4Sig < 0.S,
    (-io.in.nvMxFp4Sig).asUInt, io.in.nvMxFp4Sig.asUInt), nvSigWidth - 1)
  val fp8Clz = if (p4Add0OutNum == 8) {
    val clz = FpeArithmetic.leadingZeros(fp8Magnitude(31, 0), 32, 6)
    Mux(fp8Magnitude(32), 0.U(6.W), FpeArithmetic.truncU(clz + 1.U, 6))
  } else {
    FpeArithmetic.leadingZeros(fp8Magnitude(31, 0), 32, 6)
  }
  val p4PadWidth = 16 + 3 - log2Ceil(p4Add0OutNum)
  val nvClz = FpeArithmetic.leadingZeros(Cat(nvMagnitude, 0.U(p4PadWidth.W)), 64, 7)
  val fp8SignificantWidth = fp8Width - 1
  val nvSignificantWidth = nvSigWidth - 1
  val fp8Right = fp8SignificantWidth.U > fp8Clz + 26.U
  val fp8RightAmount = fp8SignificantWidth.U - fp8Clz - 26.U
  val fp8LeftAmount = fp8Clz + 26.U - fp8SignificantWidth.U
  val fp8Sig = Mux(fp8Right,
    FpeArithmetic.truncU(fp8Magnitude >> fp8RightAmount, 26),
    FpeArithmetic.truncU(fp8Magnitude << fp8LeftAmount, 26))
  val nvRight = nvSignificantWidth.U > nvClz + 36.U
  val nvRightAmount = nvSignificantWidth.U - nvClz - 36.U
  val nvLeftAmount = nvClz + 36.U - nvSignificantWidth.U
  val nvSig = Mux(nvRight,
    FpeArithmetic.truncU(nvMagnitude >> nvRightAmount, 36),
    FpeArithmetic.truncU(nvMagnitude << nvLeftAmount, 36))
  val fp8Exp = FpeArithmetic.truncS(
    io.in.eMax + fp8SignificantWidth.S(10.W) - fp8Clz.pad(10).asSInt - 26.S(10.W), 10)
  val nvExp = FpeArithmetic.truncS(
    io.in.blockMax + nvSignificantWidth.S(10.W) - nvClz.pad(10).asSInt - 36.S(10.W), 10)

  io.out := 0.U.asTypeOf(io.out)
  io.out.nan := io.in.nan
  io.out.pInf := io.in.pInf
  io.out.nInf := io.in.nInf
  io.out.intResult := io.in.intResult
  io.out.int32 := io.in.signFp8(31, 0)
  io.out.fp32Sign := Mux(io.in.mxIntFp4, io.in.nvMxFp4Sig.asUInt(nvSigWidth - 1),
    io.in.signFp8.asUInt(fp8Width - 1))
  io.out.fp32Sig := Mux(io.in.mxIntFp4, nvSig(35, 12), fp8Sig(25, 2))
  io.out.fp32Exp := Mux(io.in.mxIntFp4, nvExp, fp8Exp)
}

/** Package normal, denormal, integer, and exceptional results in pipe8. */
class FReduceMACPipe8(implicit p: Parameters) extends CuteFpeModule {
  val io = IO(new Bundle {
    val in = Input(new FPipe7Result)
    val out = Output(new FPipe8Result)
  })

  val expBits = io.in.fp32Exp.asUInt
  val negativeExponent = io.in.fp32Exp < 0.S
  val exponent = Mux(negativeExponent || !io.in.fp32Sig.orR, 0.U(8.W),
    Mux(expBits(8), "hff".U(8.W), expBits(7, 0)))
  val denormalShift = FpeArithmetic.truncU((1.S(11.W) - io.in.fp32Exp.pad(11)).asUInt, 11)
  val fraction = Mux(negativeExponent || !expBits.orR,
    FpeArithmetic.truncU(io.in.fp32Sig >> denormalShift, 23),
    Mux(expBits(8) || expBits(7, 0).andR, 0.U(23.W), io.in.fp32Sig(22, 0)))
  val regular = Cat(io.in.fp32Sign, exponent, fraction)
  val exceptional = Mux(io.in.nan, "h7fffffff".U(32.W),
    Mux(io.in.nInf, "hff800000".U(32.W),
      Mux(io.in.pInf, "h7f800000".U(32.W), regular)))
  val floating = Mux(exceptional(30, 0).orR, exceptional, 0.U(32.W))

  io.out.result := Mux(io.in.intResult, io.in.int32, floating)
}
