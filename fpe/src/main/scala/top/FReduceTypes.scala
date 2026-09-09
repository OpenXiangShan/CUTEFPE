package cute

import chisel3._

object FReduceComputeType {
    val ComputeTypeBitWidth = 4
    val ComputeTypeUndef = 15.U(ComputeTypeBitWidth.W)
    val I8I8I32 = 0.U(ComputeTypeBitWidth.W)
    val F16F16F32 = 1.U(ComputeTypeBitWidth.W)
    val BF16BF16F32 = 2.U(ComputeTypeBitWidth.W)
    val TF32TF32F32 = 3.U(ComputeTypeBitWidth.W)
    val I8U8I32 = 4.U(ComputeTypeBitWidth.W)
    val U8I8I32 = 5.U(ComputeTypeBitWidth.W)
    val U8U8I32 = 6.U(ComputeTypeBitWidth.W)
    val Mxfp8e4m3F32 = 7.U(ComputeTypeBitWidth.W)
    val Mxfp8e5m2F32 = 8.U(ComputeTypeBitWidth.W)
    val Nvfp4F32 = 9.U(ComputeTypeBitWidth.W)
    val Mxfp4F32 = 10.U(ComputeTypeBitWidth.W)
    val Fp8e4m3F32 = 11.U(ComputeTypeBitWidth.W)
    val Fp8e5m2F32 = 12.U(ComputeTypeBitWidth.W)
}

/**
  * The complete opcode namespace adds seven formats beyond the old public
  * enumeration.  Keep the four-bit object above unchanged because it is part
  * of the public FReducePE contract.  The datapath uses this five-bit
  * namespace internally and narrows only at the FReducePE boundary.
  */
object FpeOpcode {
  val Width = 5

  private def op(value: Int): UInt = value.U(Width.W)

  val I8I8I32      = op(0)
  val F16F16F32    = op(1)
  val BF16BF16F32  = op(2)
  val TF32TF32F32  = op(3)
  val I8U8I32      = op(4)
  val U8I8I32      = op(5)
  val U8U8I32      = op(6)
  val Mxfp8e4m3F32 = op(7)
  val Mxfp8e5m2F32 = op(8)
  val Nvfp4F32     = op(9)
  val Mxfp4F32     = op(10)
  val Fp8e4m3F32   = op(11)
  val Fp8e5m2F32   = op(12)
  val Mxfp6e3m2F32 = op(13)
  val Mxfp6e2m3F32 = op(14)
  val I4I4I32      = op(15)
  val I4U4I32      = op(16)
  val U4I4I32      = op(17)
  val U4U4I32      = op(18)
  val Mxint4F32    = op(19)

  def isMxfp8OrFp6(opcode: UInt): Bool =
    opcode === Mxfp8e4m3F32 || opcode === Mxfp8e5m2F32 ||
      opcode === Mxfp6e3m2F32 || opcode === Mxfp6e2m3F32

  def isMxIntFp4(opcode: UInt): Bool =
    opcode === Nvfp4F32 || opcode === Mxfp4F32 || opcode === Mxint4F32

  def requiresScale(opcode: UInt): Bool =
    isMxfp8OrFp6(opcode) || isMxIntFp4(opcode)
}

/** Common complete decode controls which cross the first two pipeline cuts. */
class FpeDecodeFlags extends Bundle {
  val highPrecision = Bool()
  val lowPrecision = Bool()
  val mxNvFp4 = Bool()
  val int8 = Bool()
  val int4 = Bool()
  val mxInt4 = Bool()
  val intFp4 = Bool()
  val mxIntFp4 = Bool()
}

/** Decoded one-side 32-bit lane fields before the complete multiplier selection. */
class FpeOperandDecode extends Bundle {
  val v8 = Vec(2, UInt(8.W))
  val v4 = Vec(2, UInt(4.W))
  val expE5 = Vec(4, UInt(5.W))
  val zero = Vec(4, Bool())
  val sign = UInt(8.W)
  val nzE5 = UInt(4.W)
  val nzE3 = UInt(4.W)
}

/** Complete decoded result for one 32-bit A/B lane pair. */
class FpeLaneDecode extends Bundle {
  val a8 = Vec(2, UInt(8.W))
  val a4 = Vec(2, UInt(4.W))
  val b4 = Vec(2, UInt(4.W))
  val b8 = Vec(2, UInt(8.W))
  val expE6 = Vec(4, UInt(6.W))
  val sign = UInt(8.W)
  val nan = Bool()
  val pInf = Bool()
  val nInf = Bool()
  val mxIntFp4AllZero = Bool()
  val mxFp86AllZero = Bool()
}

/** One 32-bit lane after the complete mantissa product packing stage. */
class FpeMulResult extends Bundle {
  val product = Vec(2, UInt(22.W))
  val expE6 = Vec(4, UInt(6.W))
}

class FpeExceptionVector extends Bundle {
  val nan = UInt(4.W)
  val inf = UInt(4.W)
  val zero = UInt(4.W)
}
