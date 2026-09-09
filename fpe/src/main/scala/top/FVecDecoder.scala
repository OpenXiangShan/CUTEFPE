package cute

import chisel3._
import chisel3.util._

/**
  * FPE input decode.  The SystemVerilog implementation splits
  * this work across decode_opcode, decode_A/Bdata, decode_int*, FP4toint,
  * assign_INTFP4, exp_sum, and exception helpers.  Keeping it as named
  * Chisel helpers preserves that dataflow while avoiding a forest of tiny
  * single-purpose Modules for every 32-bit lane.
  */
object FVecDecoder {
  private def low(value: UInt, width: Int): UInt = value.pad(width)(width - 1, 0)

  private def byte(value: UInt, index: Int): UInt =
    value(index * 8 + 7, index * 8)

  private def nibble(value: UInt, index: Int): UInt =
    value(index * 4 + 3, index * 4)

  def flags(opcode: UInt): FpeDecodeFlags = {
    val out = Wire(new FpeDecodeFlags)
    out := 0.U.asTypeOf(out)
    val isInt8 = opcode === FpeOpcode.I8I8I32 || opcode === FpeOpcode.I8U8I32 ||
      opcode === FpeOpcode.U8I8I32 || opcode === FpeOpcode.U8U8I32
    val isInt4 = opcode === FpeOpcode.I4I4I32 || opcode === FpeOpcode.I4U4I32 ||
      opcode === FpeOpcode.U4I4I32 || opcode === FpeOpcode.U4U4I32
    val isMxNvFp4 = opcode === FpeOpcode.Nvfp4F32 || opcode === FpeOpcode.Mxfp4F32
    val isMxInt4 = opcode === FpeOpcode.Mxint4F32

    out.highPrecision := opcode === FpeOpcode.TF32TF32F32 ||
      opcode === FpeOpcode.BF16BF16F32 || opcode === FpeOpcode.F16F16F32
    out.lowPrecision := opcode === FpeOpcode.Mxfp8e5m2F32 ||
      opcode === FpeOpcode.Fp8e5m2F32 || opcode === FpeOpcode.Mxfp6e3m2F32 ||
      opcode === FpeOpcode.Mxfp8e4m3F32 || opcode === FpeOpcode.Fp8e4m3F32 ||
      opcode === FpeOpcode.Mxfp6e2m3F32
    out.mxNvFp4 := isMxNvFp4
    out.int8 := isInt8
    out.int4 := isInt4
    out.mxInt4 := isMxInt4
    out.intFp4 := isInt4 || isMxNvFp4 || isMxInt4
    out.mxIntFp4 := isMxNvFp4 || isMxInt4
    out
  }

  private def isTf32(opcode: UInt): Bool = opcode === FpeOpcode.TF32TF32F32
  private def isBf16(opcode: UInt): Bool = opcode === FpeOpcode.BF16BF16F32
  private def isFp16(opcode: UInt): Bool = opcode === FpeOpcode.F16F16F32
  private def isE5(opcode: UInt): Bool =
    opcode === FpeOpcode.Mxfp8e5m2F32 || opcode === FpeOpcode.Fp8e5m2F32
  private def isE4(opcode: UInt): Bool =
    opcode === FpeOpcode.Mxfp8e4m3F32 || opcode === FpeOpcode.Fp8e4m3F32
  private def isE3(opcode: UInt): Bool = opcode === FpeOpcode.Mxfp6e3m2F32
  private def isE2(opcode: UInt): Bool = opcode === FpeOpcode.Mxfp6e2m3F32

  private def signedA(opcode: UInt): Bool =
    opcode === FpeOpcode.I8I8I32 || opcode === FpeOpcode.I8U8I32 ||
      opcode === FpeOpcode.I4I4I32 || opcode === FpeOpcode.I4U4I32 ||
      opcode === FpeOpcode.Mxint4F32

  private def signedB(opcode: UInt): Bool =
    opcode === FpeOpcode.I8I8I32 || opcode === FpeOpcode.U8I8I32 ||
      opcode === FpeOpcode.I4I4I32 || opcode === FpeOpcode.U4I4I32 ||
      opcode === FpeOpcode.Mxint4F32

  private def absWhenSigned(value: UInt, signed: Bool): UInt = {
    val magnitude = low(~value + 1.U, value.getWidth)
    Mux(signed && value(value.getWidth - 1), magnitude, value)
  }

  private def decodeOperand(value: UInt, opcode: UInt, signed: Bool): FpeOperandDecode = {
    val out = Wire(new FpeOperandDecode)
    out := 0.U.asTypeOf(out)

    val tf32 = isTf32(opcode)
    val bf16 = isBf16(opcode)
    val fp16 = isFp16(opcode)
    val e5 = isE5(opcode)
    val e4 = isE4(opcode)
    val e3 = isE3(opcode)
    val e2 = isE2(opcode)
    val f = flags(opcode)

    val nzTf32 = value(30, 23).orR
    val nzFp16 = Wire(Vec(2, Bool()))
    nzFp16(0) := value(14, 10).orR
    nzFp16(1) := value(30, 26).orR
    val nzBf16 = Wire(Vec(2, Bool()))
    nzBf16(0) := value(14, 7).orR
    nzBf16(1) := nzTf32
    val nzE5 = Wire(Vec(4, Bool()))
    val nzE4 = Wire(Vec(4, Bool()))
    val nzE3 = Wire(Vec(4, Bool()))
    val nzE2 = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
      nzE5(i) := value(i * 8 + 6, i * 8 + 2).orR
      nzE4(i) := value(i * 8 + 6, i * 8 + 3).orR
      nzE3(i) := value(i * 8 + 4, i * 8 + 2).orR
      nzE2(i) := value(i * 8 + 4, i * 8 + 3).orR
    }
    out.nzE5 := nzE5.asUInt
    out.nzE3 := nzE3.asUInt

    val tf32Exp = Mux(nzTf32, value(30, 23), 1.U(8.W))
    val fp16Exp = Wire(Vec(2, UInt(5.W)))
    fp16Exp(0) := Mux(nzFp16(0), value(14, 10), 1.U(5.W))
    fp16Exp(1) := Mux(nzFp16(1), value(30, 26), 1.U(5.W))
    val bf16Exp0 = Mux(nzBf16(0), value(14, 7), 1.U(8.W))
    val e5Exp = Wire(Vec(4, UInt(5.W)))
    val e4Exp = Wire(Vec(4, UInt(4.W)))
    val e3Exp = Wire(Vec(4, UInt(3.W)))
    val e2Exp = Wire(Vec(4, UInt(2.W)))
    for (i <- 0 until 4) {
      e5Exp(i) := Mux(nzE5(i), value(i * 8 + 6, i * 8 + 2), 1.U(5.W))
      e4Exp(i) := Mux(nzE4(i), value(i * 8 + 6, i * 8 + 3), 1.U(4.W))
      e3Exp(i) := Mux(nzE3(i), value(i * 8 + 4, i * 8 + 2), 1.U(3.W))
      e2Exp(i) := Mux(nzE2(i), value(i * 8 + 4, i * 8 + 3), 1.U(2.W))
    }

    out.zero(3) := Mux(e5 || e4, !value(30, 24).orR, !value(28, 24).orR)
    out.zero(2) := Mux(e5 || e4, !value(22, 16).orR, !value(20, 16).orR)
    out.zero(1) := Mux(e5 || e4, !value(14, 8).orR,
      Mux(e3 || e2, !value(12, 8).orR,
        Mux(tf32, !value(30, 13).orR, !value(30, 16).orR)))
    out.zero(0) := Mux(e5 || e4, !value(6, 0).orR,
      Mux(e3 || e2, !value(4, 0).orR,
        Mux(tf32, true.B, !value(14, 0).orR)))

    val sign = Wire(Vec(8, Bool()))
    val unsignedInteger = (f.int8 || f.int4 || f.mxInt4) && !signed
    for (i <- 4 until 8) {
      sign(i) := Mux((f.int4 || f.mxInt4) && !signed, false.B, value(i * 4 + 3))
    }
    sign(3) := Mux(unsignedInteger, false.B,
      Mux(f.intFp4, value(15), Mux(e2 || e3, value(29), value(31))))
    sign(2) := Mux(unsignedInteger, false.B,
      Mux(f.intFp4, value(11), Mux(e2 || e3, value(21), value(23))))
    sign(1) := Mux(bf16 || fp16 || tf32, value(31),
      Mux(unsignedInteger, false.B,
        Mux(f.intFp4, value(7), Mux(e2 || e3, value(13), value(15)))))
    sign(0) := Mux(bf16 || fp16, value(15),
      Mux(unsignedInteger, false.B,
        Mux(f.intFp4, value(3), Mux(e2 || e3, value(5), value(7)))))
    out.sign := sign.asUInt

    out.expE5(0) := MuxCase(0.U(5.W), Seq(
      bf16 -> Cat(0.U(2.W), bf16Exp0(2, 0)),
      fp16 -> Cat(0.U(2.W), fp16Exp(0)(2, 0)),
      e5 -> e5Exp(0),
      e4 -> Cat(0.U(1.W), e4Exp(0)),
      e3 -> Cat(0.U(2.W), e3Exp(0)),
      e2 -> Cat(0.U(3.W), e2Exp(0))
    ))
    out.expE5(1) := MuxCase(Cat(0.U(3.W), e2Exp(1)), Seq(
      (tf32 || bf16) -> Cat(0.U(2.W), tf32Exp(2, 0)),
      fp16 -> Cat(0.U(2.W), fp16Exp(1)(2, 0)),
      f.int8 -> 0.U(5.W),
      e5 -> e5Exp(1),
      e4 -> Cat(0.U(1.W), e4Exp(1)),
      e3 -> Cat(0.U(2.W), e3Exp(1))
    ))
    out.expE5(2) := MuxCase(bf16Exp0(7, 3), Seq(
      e5 -> e5Exp(2),
      e4 -> Cat(0.U(1.W), e4Exp(2)),
      e3 -> Cat(0.U(2.W), e3Exp(2)),
      e2 -> Cat(0.U(3.W), e2Exp(2)),
      fp16 -> Cat(0.U(3.W), fp16Exp(0)(4, 3)),
      (tf32 || f.int8) -> 0.U(5.W)
    ))
    out.expE5(3) := MuxCase(tf32Exp(7, 3), Seq(
      f.int8 -> 0.U(5.W),
      e5 -> e5Exp(3),
      e4 -> Cat(0.U(1.W), e4Exp(3)),
      e3 -> Cat(0.U(2.W), e3Exp(3)),
      fp16 -> Cat(0.U(3.W), fp16Exp(1)(4, 3)),
      e2 -> Cat(0.U(3.W), e2Exp(3))
    ))

    val bf16Sig0 = value(6, 0)
    val bf16Sig1 = value(22, 16)
    val fp16Sig0 = value(9, 0)
    val fp16Sig1 = value(25, 16)
    val tf32Sig = value(22, 13)
    out.v8(0) := MuxCase(Cat(0.U(4.W), nzE2(0), value(2, 0)), Seq(
      bf16 -> Cat(0.U(1.W), nzBf16(0), bf16Sig0(6, 1)),
      tf32 -> 0.U(8.W),
      fp16 -> Cat(0.U(1.W), nzFp16(0), fp16Sig0(9, 4)),
      e5 -> Cat(0.U(4.W), nzE5(0), value(1, 0), 0.U(1.W)),
      e3 -> Cat(0.U(4.W), nzE3(0), value(1, 0), 0.U(1.W)),
      e4 -> Cat(0.U(4.W), nzE4(0), value(2, 0))
    ))
    out.v8(1) := MuxCase(Cat(0.U(4.W), nzE2(2), value(18, 16)), Seq(
      bf16 -> Cat(0.U(1.W), nzBf16(1), bf16Sig1(6, 1)),
      fp16 -> Cat(0.U(1.W), nzFp16(1), fp16Sig1(9, 4)),
      tf32 -> Cat(0.U(1.W), nzTf32, tf32Sig(9, 4)),
      e5 -> Cat(0.U(4.W), nzE5(2), value(17, 16), 0.U(1.W)),
      e3 -> Cat(0.U(4.W), nzE3(2), value(17, 16), 0.U(1.W)),
      e4 -> Cat(0.U(4.W), nzE4(2), value(18, 16))
    ))
    out.v4(0) := MuxCase(Cat(nzE2(1), value(10, 8)), Seq(
      fp16 -> fp16Sig0(3, 0),
      bf16 -> Cat(bf16Sig0(0), 0.U(3.W)),
      tf32 -> 0.U(4.W),
      e5 -> Cat(nzE5(1), value(9, 8), 0.U(1.W)),
      e3 -> Cat(nzE3(1), value(9, 8), 0.U(1.W)),
      e4 -> Cat(nzE4(1), value(10, 8))
    ))
    out.v4(1) := MuxCase(Cat(nzE2(3), value(26, 24)), Seq(
      fp16 -> fp16Sig1(3, 0),
      bf16 -> Cat(bf16Sig1(0), 0.U(3.W)),
      tf32 -> tf32Sig(3, 0),
      e5 -> Cat(nzE5(3), value(25, 24), 0.U(1.W)),
      e3 -> Cat(nzE3(3), value(25, 24), 0.U(1.W)),
      e4 -> Cat(nzE4(3), value(26, 24))
    ))
    out
  }

  private def fp4ToInt(value: UInt): Vec[UInt] = {
    val out = Wire(Vec(8, UInt(4.W)))
    for (i <- 0 until 8) {
      val element = nibble(value, i)
      val exp = element(2, 1)
      val sig = Cat(0.U(2.W), exp.orR, element(0))
      val shifted = MuxLookup(exp, sig)(Seq(
        2.U(2.W) -> low(sig << 1, 4),
        3.U(2.W) -> low(sig << 2, 4)
      ))
      out(i) := shifted
    }
    out
  }

  private def int4Magnitude(value: UInt, signed: Bool): Vec[UInt] = {
    val out = Wire(Vec(8, UInt(4.W)))
    for (i <- 0 until 8) {
      out(i) := absWhenSigned(nibble(value, i), signed)
    }
    out
  }

  private def exceptionVector(value: UInt, opcode: UInt): FpeExceptionVector = {
    val out = Wire(new FpeExceptionVector)
    out := 0.U.asTypeOf(out)
    val nanBits = Wire(Vec(4, Bool()))
    val infBits = Wire(Vec(4, Bool()))
    val zeroBits = Wire(Vec(4, Bool()))
    val tf32 = isTf32(opcode)
    val bf16 = isBf16(opcode)
    val fp16 = isFp16(opcode)
    val e5 = isE5(opcode)
    val e4 = isE4(opcode)

    def nan(exp: UInt, sig: UInt): Bool = exp.andR && sig.orR
    def inf(exp: UInt, sig: UInt): Bool = exp.andR && !sig.orR
    def zero(exp: UInt, sig: UInt): Bool = !exp.orR && !sig.orR

    val tf32Exp = value(30, 23)
    val tf32Sig = value(22, 13)
    val bf16Exp0 = value(14, 7)
    val bf16Sig0 = value(6, 0)
    val bf16Exp1 = value(30, 23)
    val bf16Sig1 = value(22, 16)
    val fp16Exp0 = value(14, 10)
    val fp16Sig0 = value(9, 0)
    val fp16Exp1 = value(30, 26)
    val fp16Sig1 = value(25, 16)

    nanBits(0) := Mux(bf16, nan(bf16Exp0, bf16Sig0),
      Mux(fp16, nan(fp16Exp0, fp16Sig0),
        Mux(e5, nan(value(6, 2), value(1, 0)), Mux(e4, value(6, 3).andR && value(2, 0).andR, false.B))))
    nanBits(1) := Mux(tf32, nan(tf32Exp, tf32Sig),
      Mux(bf16, nan(bf16Exp1, bf16Sig1), Mux(fp16, nan(fp16Exp1, fp16Sig1),
        Mux(e5, nan(value(14, 10), value(9, 8)), Mux(e4, value(14, 11).andR && value(10, 8).andR, false.B)))))
    nanBits(2) := Mux(e5, nan(value(22, 18), value(17, 16)),
      Mux(e4, value(22, 19).andR && value(18, 16).andR, false.B))
    nanBits(3) := Mux(e5, nan(value(30, 26), value(25, 24)),
      Mux(e4, value(30, 27).andR && value(26, 24).andR, false.B))

    infBits(0) := Mux(bf16, inf(bf16Exp0, bf16Sig0), Mux(fp16, inf(fp16Exp0, fp16Sig0),
      Mux(e5, inf(value(6, 2), value(1, 0)), false.B)))
    infBits(1) := Mux(tf32, inf(tf32Exp, tf32Sig), Mux(bf16, inf(bf16Exp1, bf16Sig1),
      Mux(fp16, inf(fp16Exp1, fp16Sig1), Mux(e5, inf(value(14, 10), value(9, 8)), false.B))))
    infBits(2) := Mux(e5, inf(value(22, 18), value(17, 16)), false.B)
    infBits(3) := Mux(e5, inf(value(30, 26), value(25, 24)), false.B)

    zeroBits(0) := Mux(bf16, zero(bf16Exp0, bf16Sig0), Mux(fp16, zero(fp16Exp0, fp16Sig0),
      Mux(e5, zero(value(6, 2), value(1, 0)), false.B)))
    zeroBits(1) := Mux(tf32, zero(tf32Exp, tf32Sig), Mux(bf16, zero(bf16Exp1, bf16Sig1),
      Mux(fp16, zero(fp16Exp1, fp16Sig1), Mux(e5, zero(value(14, 10), value(9, 8)), false.B))))
    zeroBits(2) := Mux(e5, zero(value(22, 18), value(17, 16)), false.B)
    zeroBits(3) := Mux(e5, zero(value(30, 26), value(25, 24)), false.B)
    out.nan := nanBits.asUInt
    out.inf := infBits.asUInt
    out.zero := zeroBits.asUInt
    out
  }

  def decodeLane(aVector: UInt, bVector: UInt, opcode: UInt): FpeLaneDecode = {
    val out = Wire(new FpeLaneDecode)
    out := 0.U.asTypeOf(out)
    val f = flags(opcode)
    val aSigned = signedA(opcode)
    val bSigned = signedB(opcode)
    val a = decodeOperand(aVector, opcode, aSigned)
    val b = decodeOperand(bVector, opcode, bSigned)

    val int8A = Wire(Vec(4, UInt(8.W)))
    val int8B = Wire(Vec(4, UInt(8.W)))
    for (i <- 0 until 4) {
      int8A(i) := absWhenSigned(byte(aVector, i), aSigned)
      int8B(i) := absWhenSigned(byte(bVector, i), bSigned)
    }
    val int8A8 = Wire(Vec(2, UInt(8.W)))
    val int8A4 = Wire(Vec(2, UInt(4.W)))
    val int8B8 = Wire(Vec(2, UInt(8.W)))
    val int8B4 = Wire(Vec(2, UInt(4.W)))
    val int8Exp = Wire(Vec(4, UInt(6.W)))
    int8A8(0) := int8A(0)
    int8B8(0) := int8B(0)
    int8A4(0) := int8A(1)(7, 4)
    int8B4(0) := int8B(1)(7, 4)
    int8Exp(0) := Cat(0.U(2.W), int8A(1)(3, 0))
    int8Exp(2) := Cat(0.U(2.W), int8B(1)(3, 0))
    int8A8(1) := int8A(2)
    int8B8(1) := int8B(2)
    int8A4(1) := int8A(3)(7, 4)
    int8B4(1) := int8B(3)(7, 4)
    int8Exp(1) := Cat(0.U(2.W), int8A(3)(3, 0))
    int8Exp(3) := Cat(0.U(2.W), int8B(3)(3, 0))

    val fp4A = fp4ToInt(aVector)
    val fp4B = fp4ToInt(bVector)
    val int4A = int4Magnitude(aVector, aSigned)
    val int4B = int4Magnitude(bVector, bSigned)
    val sourceA = Wire(Vec(8, UInt(4.W)))
    val sourceB = Wire(Vec(8, UInt(4.W)))
    for (i <- 0 until 8) {
      sourceA(i) := Mux(f.mxNvFp4, fp4A(i), int4A(i))
      sourceB(i) := Mux(f.mxNvFp4, fp4B(i), int4B(i))
    }
    val intFpA8 = Wire(Vec(2, UInt(8.W)))
    val intFpA4 = Wire(Vec(2, UInt(4.W)))
    val intFpB8 = Wire(Vec(2, UInt(8.W)))
    val intFpB4 = Wire(Vec(2, UInt(4.W)))
    val intFpExp = Wire(Vec(4, UInt(6.W)))
    intFpA8(0) := Cat(sourceA(1), sourceA(0))
    intFpB8(0) := Cat(sourceB(1), sourceB(0))
    intFpA4(0) := sourceA(2)
    intFpB4(0) := sourceB(2)
    intFpExp(0) := Cat(0.U(2.W), sourceA(3))
    intFpExp(2) := Cat(0.U(2.W), sourceB(3))
    intFpA8(1) := Cat(sourceA(5), sourceA(4))
    intFpB8(1) := Cat(sourceB(5), sourceB(4))
    intFpA4(1) := sourceA(6)
    intFpB4(1) := sourceB(6)
    intFpExp(1) := Cat(0.U(2.W), sourceA(7))
    intFpExp(3) := Cat(0.U(2.W), sourceB(7))

    for (i <- 0 until 2) {
      out.a8(i) := Mux(f.int8, int8A8(i), Mux(f.intFp4, intFpA8(i), a.v8(i)))
      out.a4(i) := Mux(f.int8, int8A4(i), Mux(f.intFp4, intFpA4(i), a.v4(i)))
      out.b8(i) := Mux(f.int8, int8B8(i), Mux(f.intFp4, intFpB8(i), b.v8(i)))
      out.b4(i) := Mux(f.int8, int8B4(i), Mux(f.intFp4, intFpB4(i), b.v4(i)))
    }

    val fpExp = Wire(Vec(4, UInt(6.W)))
    val zero = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
      zero(i) := a.zero(i) || b.zero(i)
    }
    val e9_0 = low(Cat(a.expE5(2), a.expE5(0)(2, 0)) +& Cat(b.expE5(2), b.expE5(0)(2, 0)), 9)
    val e9_1 = low(Cat(a.expE5(3), a.expE5(1)(2, 0)) +& Cat(b.expE5(3), b.expE5(1)(2, 0)), 9)
    val e6 = Wire(Vec(4, UInt(6.W)))
    for (i <- 0 until 4) {
      e6(i) := Mux(zero(i), 0.U(6.W), low(a.expE5(i) +& b.expE5(i), 6))
    }
    val useE9 = isTf32(opcode) || isBf16(opcode) || isFp16(opcode)
    fpExp(0) := Mux(zero(0), 0.U(6.W), Mux(useE9, Cat(0.U(3.W), e9_0(2, 0)), e6(0)))
    fpExp(1) := Mux(zero(1), 0.U(6.W), Mux(useE9, Cat(0.U(3.W), e9_1(2, 0)), e6(1)))
    // In the high-precision path expE6(0)/expE6(2) are the low/high
    // fragments of one E9 exponent, and likewise for 1/3.  Low-precision
    // E6 elements remain independent, including their zero predicates.
    fpExp(2) := Mux(useE9, Mux(zero(0), 0.U(6.W), e9_0(8, 3)), e6(2))
    fpExp(3) := Mux(useE9, Mux(zero(1), 0.U(6.W), e9_1(8, 3)), e6(3))
    for (i <- 0 until 4) {
      out.expE6(i) := Mux(f.int8, int8Exp(i), Mux(f.intFp4, intFpExp(i), fpExp(i)))
    }
    out.sign := a.sign ^ b.sign

    val lowANz = Wire(Vec(4, Bool()))
    val lowBNz = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
      lowANz(i) := aVector(i * 8 + 1, i * 8).orR
      lowBNz(i) := bVector(i * 8 + 1, i * 8).orR
    }
    val fp8NonZero = Wire(Vec(4, Bool()))
    val fp6NonZero = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
      fp8NonZero(i) := (a.nzE5(i) || lowANz(i)) && (b.nzE5(i) || lowBNz(i))
      fp6NonZero(i) := (a.nzE3(i) || lowANz(i)) && (b.nzE3(i) || lowBNz(i))
    }
    val fp8AllZero = !fp8NonZero.asUInt.orR
    val fp6AllZero = !fp6NonZero.asUInt.orR
    out.mxFp86AllZero := Mux(opcode === FpeOpcode.Mxfp8e4m3F32 || opcode === FpeOpcode.Mxfp8e5m2F32,
      fp8AllZero, fp6AllZero)

    val fp4ANz = Wire(Vec(8, Bool()))
    val fp4BNz = Wire(Vec(8, Bool()))
    for (i <- 0 until 8) {
      val lane = i / 2
      val lowOffset = lane * 8 + (if ((i & 1) == 0) 0 else 4)
      fp4ANz(i) := (if ((i & 1) == 0) aVector(lowOffset + 2) || lowANz(lane) else aVector(lowOffset + 2, lowOffset).orR)
      fp4BNz(i) := (if ((i & 1) == 0) bVector(lowOffset + 2) || lowBNz(lane) else bVector(lowOffset + 2, lowOffset).orR)
    }
    val mxIntANz = Wire(Vec(8, Bool()))
    val mxIntBNz = Wire(Vec(8, Bool()))
    for (i <- 0 until 8) {
      mxIntANz(i) := aVector(i * 4 + 3) || fp4ANz(i)
      mxIntBNz(i) := bVector(i * 4 + 3) || fp4BNz(i)
    }
    val fp4PairNonZero = Wire(Vec(8, Bool()))
    val mxIntPairNonZero = Wire(Vec(8, Bool()))
    for (i <- 0 until 8) {
      fp4PairNonZero(i) := fp4ANz(i) && fp4BNz(i)
      mxIntPairNonZero(i) := mxIntANz(i) && mxIntBNz(i)
    }
    out.mxIntFp4AllZero := Mux(f.mxInt4, !mxIntPairNonZero.asUInt.orR, !fp4PairNonZero.asUInt.orR)

    val aException = exceptionVector(aVector, opcode)
    val bException = exceptionVector(bVector, opcode)
    val nanPerElement = Wire(Vec(4, Bool()))
    val pInfPerElement = Wire(Vec(4, Bool()))
    val nInfPerElement = Wire(Vec(4, Bool()))
    for (i <- 0 until 4) {
      val nan = aException.nan(i) || bException.nan(i) ||
        (aException.zero(i) && bException.inf(i)) || (bException.zero(i) && aException.inf(i))
      val anyInf = aException.inf(i) || bException.inf(i)
      nanPerElement(i) := nan
      pInfPerElement(i) := anyInf && !out.sign(i) && !nan
      nInfPerElement(i) := anyInf && out.sign(i) && !nan
    }
    val anyNan = nanPerElement.asUInt.orR
    val anyPInf = pInfPerElement.asUInt.orR
    val anyNInf = nInfPerElement.asUInt.orR
    out.nan := anyNan || (anyPInf && anyNInf)
    out.pInf := !out.nan && anyPInf
    out.nInf := !out.nan && anyNInf
    out
  }
}
