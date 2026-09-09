package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

// Payloads cross the nine explicit register boundaries of the FPE pipeline.
// Their names follow the original FPipe*Result convention, while their fields
// are defined by the current stage boundaries.

class FPipe0Result(implicit p: Parameters) extends CuteFpeBundle {
  private val laneNum = ReduceWidth / 32
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize
  private val mxfp86ZeroGroupSize = if (laneNum >= 8) 8 else laneNum
  private val mxfp86ZeroGroupNum = laneNum / mxfp86ZeroGroupSize

  val cAdd = UInt(ResultWidth.W)
  val aScale = UInt((scaleNum * ScaleElementWidth).W)
  val bScale = UInt((scaleNum * ScaleElementWidth).W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val opcode = UInt(4.W)
  val flags = new FpeDecodeFlags
  val a8 = Vec(laneNum, Vec(2, UInt(8.W)))
  val a4 = Vec(laneNum, Vec(2, UInt(4.W)))
  val b4 = Vec(laneNum, Vec(2, UInt(4.W)))
  val b8 = Vec(laneNum, Vec(2, UInt(8.W)))
  val expE6 = Vec(laneNum, Vec(4, UInt(6.W)))
  val mxIntFp4AllZero = Vec(scaleNum, Bool())
  val mxFp86AllZero = Vec(mxfp86ZeroGroupNum, Bool())
  val vec8Sign = Vec(laneNum, UInt(8.W))
}

class FPipe1Result(implicit p: Parameters) extends CuteFpeBundle {
  private val laneNum = ReduceWidth / 32
  private val blockNum = laneNum / 2
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize

  val cAdd = UInt(ResultWidth.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val flags = new FpeDecodeFlags
  val scaleType = Bool()
  val thresholdNormal = Bool()
  val thresholdMxIntFp4 = Bool()
  val bias = SInt(8.W)
  val scaleExp = Vec(scaleNum, UInt(9.W))
  val scaleSig = Vec(scaleNum, UInt(8.W))
  val product = Vec(laneNum, Vec(2, UInt(22.W)))
  val expE6 = Vec(laneNum, Vec(4, UInt(6.W)))
  // Pipe1 preserves the partial comparison state consumed by Pipe2.  The
  // first index selects the lane block and the second selects exponent stream.
  val candidateMask = Vec(2, Vec(2, UInt(blockNum.W)))
  val remainingE3 = Vec(2, Vec(2, Vec(3, UInt(blockNum.W))))
  val e3Max = Vec(2, Vec(2, UInt(3.W)))
  val e6Max = Vec(2, Vec(2, UInt(6.W)))
  val vec4Sign = Vec(laneNum, UInt(4.W))
}

class FPipe2Result(implicit p: Parameters) extends CuteFpeBundle {
  private val laneNum = ReduceWidth / 32
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize

  val cExp = UInt(8.W)
  val cSig = UInt(32.W)
  val cSign = Bool()
  val cZero = Bool()
  val eabMax0 = UInt(9.W)
  val eabMax1 = UInt(9.W)
  val scaleMax = UInt(9.W)
  val thresholdMxIntFp4Enable = Bool()
  val thresholdNormalExp0 = UInt(10.W)
  val thresholdNormalExp1 = UInt(10.W)
  val scaleExpS0 = SInt(9.W)
  val scaleExpS1 = SInt(9.W)
  val bias = SInt(8.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val flags = new FpeDecodeFlags
  val scaleType = Bool()
  val scaleExp = Vec(scaleNum, UInt(9.W))
  val scaleSig = Vec(scaleNum, UInt(8.W))
  val product = Vec(laneNum, Vec(2, UInt(22.W)))
  val expE6 = Vec(laneNum, Vec(4, UInt(6.W)))
  val vec4Sign = Vec(laneNum, UInt(4.W))
}

class FPipe3Result(implicit p: Parameters) extends CuteFpeBundle {
  private val laneNum = ReduceWidth / 32
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize

  val blockMax = SInt(10.W)
  val blockShift = Vec(scaleNum, UInt(6.W))
  val nvcShift = UInt(6.W)
  val eMax = SInt(10.W)
  val cShift = UInt(5.W)
  val expShift = Vec(laneNum, Vec(4, UInt(5.W)))
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val cSig = UInt(32.W)
  val cSign = Bool()
  val scaleSig = Vec(scaleNum, UInt(8.W))
  val product = Vec(laneNum, Vec(2, UInt(22.W)))
  val vec4Sign = Vec(laneNum, UInt(4.W))
  val flags = new FpeDecodeFlags
}

class FPipe4Result(implicit p: Parameters) extends CuteFpeBundle {
  private val scaleNum = ReduceWidth / MinDataTypeWidth / MinGroupSize
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val add0Width = 28 + log2Ceil(4) + log2Ceil(2)

  val signAfterAdd0 = Vec(p4Add0OutNum, SInt(add0Width.W))
  val blockMax = SInt(10.W)
  val blockShift = Vec(scaleNum, UInt(6.W))
  val nvcShift = UInt(6.W)
  val eMax = SInt(10.W)
  val cShift = UInt(5.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val cSig = UInt(32.W)
  val cSign = Bool()
  val scaleSig = Vec(scaleNum, UInt(8.W))
  val mxNvFp4 = Bool()
  val int8 = Bool()
  val int4 = Bool()
  val mxInt4 = Bool()
}

class FPipe5Result(implicit p: Parameters) extends CuteFpeBundle {
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val addNum2 = 4 / (512 / ReduceWidth)
  private val add1Width = 28 + log2Ceil(4) + log2Ceil(2) + log2Ceil(addNum2)
  private val blockSigWidth = 35 + 10

  val signAfterAdd1 = Vec(2, SInt(add1Width.W))
  val blockSigAfterShift = Vec(p4Add0OutNum, SInt(blockSigWidth.W))
  val blockMax = SInt(10.W)
  val nvcShift = UInt(6.W)
  val eMax = SInt(10.W)
  val cShift = UInt(5.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val cSig = UInt(32.W)
  val cSign = Bool()
  val mxIntFp4 = Bool()
  val intResult = Bool()
}

class FPipe6Result(implicit p: Parameters) extends CuteFpeBundle {
  private val p4Add0OutNum = 8 / (512 / ReduceWidth)
  private val addNum2 = 4 / (512 / ReduceWidth)
  private val fp8Width = 28 + log2Ceil(4) + log2Ceil(2) + log2Ceil(addNum2) + 1
  private val nvSigWidth = 35 + 10 + log2Ceil(p4Add0OutNum) + 1

  val signFp8 = SInt(fp8Width.W)
  val nvMxFp4Sig = SInt(nvSigWidth.W)
  val eMax = SInt(10.W)
  val blockMax = SInt(10.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val mxIntFp4 = Bool()
  val intResult = Bool()
}

class FPipe7Result(implicit p: Parameters) extends CuteFpeBundle {
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val intResult = Bool()
  val int32 = UInt(32.W)
  val fp32Sign = Bool()
  val fp32Sig = UInt(24.W)
  val fp32Exp = SInt(10.W)
}

class FPipe8Result(implicit p: Parameters) extends CuteFpeBundle {
  val result = UInt(ResultWidth.W)
}
