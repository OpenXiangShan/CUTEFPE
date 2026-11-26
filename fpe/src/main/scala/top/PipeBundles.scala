package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

// MX-only metadata follows the existing pipeline token/AllowIn registers.
// A finite MX32 coefficient needs 14/42/70 signed bits for E2M1/E4M3/E5M2:
// max |Q| = 4608, 49*2^35, 49*2^63 on lattices 2^-2/-18/-32.
// Common E8M0 scales change only the exponent, never this coefficient.
class MXBlockClass extends Bundle {
    val isNaN = Bool()
    val isInf = Bool()
    val infSign = Bool()
    val negativeZero = Bool()
    val invalid = Bool()
}

class MXPipeMeta(implicit p: Parameters) extends CuteFpeBundle {
    private val blocks = if (cuteFpeConfig.enableMxfp4Fp32) ReduceWidth / 128 else ReduceWidth / 256
    val c = UInt(32.W)
    val scale = Vec(blocks, SInt(9.W)) // sA+sB-254 in [-254,254].
    val classes = Vec(blocks, new MXBlockClass)
    val active = Vec(blocks, Bool())
}

class MXExactBlocks(implicit p: Parameters) extends CuteFpeBundle {
    private val blocks = if (cuteFpeConfig.enableMxfp4Fp32) ReduceWidth / 128 else ReduceWidth / 256
    private val precision = if (cuteFpeConfig.enableMxfp8Fp32) 70 else 24
    val c = UInt(32.W)
    val block = Vec(blocks, new hardfloat.RawFloat(10, precision))
    val invalid = Vec(blocks, Bool())
    val active = Vec(blocks, Bool())
}

class FPipe0Result(implicit p: Parameters) extends CuteFpeBundle{
    val mx = Option.when(cuteFpeConfig.enableMxfp8Fp32 || cuteFpeConfig.enableMxfp4Fp32)(new MXPipeMeta)
    val mxProductShift = Option.when(cuteFpeConfig.enableMxfp8Fp32)(Vec(ReduceWidth / 8, UInt(6.W)))
    val Product0 = Vec(ReduceWidth/16, SInt(23.W))
    val Product1 = Vec(ReduceWidth/16, SInt(23.W))
    val FP4ReduceRes = Option.when(cuteFpeConfig.enableFp4withsf)(
        Vec(ReduceWidth/4/FP4P0AddNum, SInt((10 + log2Ceil(FP4P0AddNum)).W))
    )
    val CMantissa = SInt(32.W)
    val ExceptionVec = Vec(ReduceWidth/8 + 1, new RawFloatException) //每个向量的异常标志位
    val MulExpVec = Vec(ReduceWidth/8 + 1, UInt(10.W))
    val CmpTreefp8P0Result = new CmpTreeP0Res(fp8cmptreelayers, ReduceWidth/8 + 1, 10)
    val SignVec = Vec(ReduceWidth/8 + 1, Bool()) //每个向量的符号位
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val scaleFP = Option.when(cuteFpeConfig.enableFp4withsf)(
        Vec(ReduceWidth/4/MinGroupSize + 1, new RawFloat(9, 11))
    )
}

class FPipe1Result(implicit p: Parameters) extends CuteFpeBundle{
    val mx = Option.when(cuteFpeConfig.enableMxfp8Fp32 || cuteFpeConfig.enableMxfp4Fp32)(new MXPipeMeta)
    val mxPartial = Option.when(cuteFpeConfig.enableMxfp8Fp32)(Vec(ReduceWidth / 64, SInt(70.W)))
    val Product0 = Vec(ReduceWidth/16, SInt(23.W))
    val Product1 = Vec(ReduceWidth/16, SInt(23.W))
    val CMantissa = SInt(32.W)
    val RightShiftVec = Vec((ReduceWidth/8) + 1, UInt(10.W))
    val FP4ReduceRes = Option.when(cuteFpeConfig.enableFp4withsf)(
      Vec(ReduceWidth/4/16, SInt((10 + log2Ceil(16)).W))
    )
    val MaxExp = UInt(10.W)
    val CmpTreefp4P0Result = Option.when(cuteFpeConfig.enableFp4withsf)(
      new CmpTreeP0Res(8, ReduceWidth/4/MinGroupSize + 1, 9)
    )
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
    val SignVec = Vec(ReduceWidth/8 + 1, Bool()) //每个向量的符号位
    val scaleFP = Option.when(cuteFpeConfig.enableFp4withsf)(
      Vec(ReduceWidth/4/MinGroupSize + 1, new RawFloat(9, 11))
    )
}

class FPipe2Result(implicit p: Parameters) extends CuteFpeBundle{
    val mx = Option.when(cuteFpeConfig.enableMxfp8Fp32 || cuteFpeConfig.enableMxfp4Fp32)(new MXExactBlocks)
    val ReduceRes0 = Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W))
    val ReduceRes1 = Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W)) // ReduceRes0是尾数右移后的结果，ReduceRes1是Int8的结果
    val CMantissa = SInt(32.W) // C的尾数
    val MaxExp = UInt(10.W)
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
    val FP4ABShift = Option.when(cuteFpeConfig.enableFp4withsf)(
      Vec(ReduceWidth/4/MinGroupSize + 1, SInt((32 + 3).W))
    )
}

class FPipe3Result(implicit p: Parameters) extends CuteFpeBundle{
    val mx = Option.when(cuteFpeConfig.enableMxfp8Fp32 || cuteFpeConfig.enableMxfp4Fp32)(new MXExactBlocks)
    val mxFlags = Option.when(cuteFpeConfig.enableMxfp8Fp32 || cuteFpeConfig.enableMxfp4Fp32)(UInt(5.W))
    // val ReduceRes = SInt(32.W) //归约计算结果
    val ReduceRes0 = SInt((32).W)
    val ReduceRes1 = SInt((32).W) // ReduceRes0是尾数右移后的结果，ReduceRes1是Int8的结果
    // val ReduceResFP4 = SInt(32.W)
    val CMantissa = SInt(32.W) // C的尾数
    val MaxExp = UInt(10.W)
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
}
