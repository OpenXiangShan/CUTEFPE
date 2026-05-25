//记得修改if相关，改成mux
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

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

class RawFloatException extends Bundle {
    val is_nan = Bool()
    val is_inf = Bool()
    val is_zero = Bool()
  }

  class RawFloat(val expWidth: Int, val precision: Int) extends Bundle {
    val sign = Bool()
    val exp = SInt(expWidth.W)
    val signed_sig = SInt((precision + 1).W)
    val exception = new RawFloatException
  }

  object RawFloat {
    // 不能用于e2m1和e4m3！这两个的异常值定义不同，e2m1没有异常值，e4m3只在0bx1111111时为NAN，指数全一为最大指数，但在这里会被处理成负数
    def fromUInt(x: UInt, expWidth: Int, pc: Int, E4M3: Boolean = false, E2M1: Boolean = false): RawFloat = {
      val fp = Wire(new RawFloat(expWidth, pc))
      val nz =  x(expWidth + pc - 2, pc - 1).orR
      val sign = x(expWidth + pc - 1)
      fp.sign := sign
      val bias = Wire(UInt(expWidth.W))
      bias := (1.U << (expWidth - 1)) - 1.U //
      fp.exp := ((x(expWidth + pc - 2, pc - 1) | !nz).asUInt - bias).asSInt
      val sig = Cat(0.U(1.W), nz, x(pc - 2, 0))
    //   fp.signed_sig := Mux(sign, -sig.asSInt.pad(pc + 1), sig.asSInt.pad(pc + 1))
      fp.signed_sig := sig.asSInt
      // 不能用e4m3
    //   if (E4M3) {
    //     fp.exception.is_nan := x(expWidth + pc - 2, 0).andR 
    //     fp.exception.is_inf := false.B
    //   }
    //   else 
      if (E2M1) {
        fp.exception.is_nan := false.B
        fp.exception.is_inf := false.B
      }
      else {
        fp.exception.is_nan := x(expWidth + pc - 2, pc - 1).andR && x(pc - 2, 0).orR
        fp.exception.is_inf := x(expWidth + pc - 2, pc - 1).andR && !x(pc - 2, 0).orR
      }
      fp.exception.is_zero := !x(expWidth + pc - 2, 0).orR
      fp
    }

    // e4m3单独处理，在计算时用5位表示exp
    def frome4m3(x: UInt): RawFloat = {
        val fp = Wire(new RawFloat(5, 4))
        val nz =  x(4 + 4 - 2, 4 - 1).orR
        val sign = x(4 + 4 - 1)
        fp.sign := sign
        val bias = Wire(UInt(5.W))
        bias := (1.U << (4 - 1)) - 1.U //
        fp.exp := ((x(4 + 4 - 2, 4 - 1) | !nz).asUInt.pad(5) - bias).asSInt
        val sig = Cat(0.U(1.W), nz, x(4 - 2, 0))
        //   fp.signed_sig := Mux(sign, -sig.asSInt.pad(pc + 1), sig.asSInt.pad(pc + 1))
        fp.signed_sig := sig.asSInt

        fp.exception.is_nan := x(4 + 4 - 2, 0).andR 
        fp.exception.is_inf := false.B
        fp.exception.is_zero := !x(4 + 4 - 2, 0).orR
        fp
    }
}

// Transform FP4 to fixed point for accumulation
class FP4toint(implicit p: Parameters) extends CuteModule {
    // Output format is xxxx.x
    // For example, 00001 represents 0.5
    val io = IO(new Bundle{
        val in = Input(UInt(4.W))
        val out = Output(SInt(5.W))
    })

    val sign = io.in(3)
    val exp = io.in(2, 1)
    val mantissa = io.in(0)
    val nz = exp.orR
    val sig = Cat(0.U(3.W), nz, mantissa).asSInt // 5位无符号数
    val unsignedSig = Wire(SInt(5.W))

    when(exp === 2.U){
        unsignedSig := sig << 1.U
    }.elsewhen(exp === 3.U){
        unsignedSig := sig << 2.U
    }.otherwise{
        unsignedSig := sig
    }

    io.out := Mux(sign, -unsignedSig.asSInt, unsignedSig.asSInt)
}

class CLZ(len: Int)(implicit p: Parameters) extends CuteModule {

  val inWidth = len
  val outWidth = (inWidth - 1).U.getWidth

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  io.out := PriorityEncoder(io.in.asBools.reverse)
}

class FDecodeResult(implicit p: Parameters) extends CuteBundle{
    // val Int8Vec = Vec(ReduceWidth/8, UInt(9.W))
    val TF32Vec = Vec(ReduceWidth/8, new RawFloat(10, 11))
}

class FPScaleDecoder(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(Vec(ReduceWidth/4/MinGroupSize, UInt(8.W)))
        val out = Output(Vec(ReduceWidth/4/MinGroupSize, new RawFloat(8, 11)))
    })

    for(i <- 0 until ReduceWidth/4/MinGroupSize){
        val Bits8  = io.in(i)
        val DecodeE4M3 = RawFloat.frome4m3(Bits8)
        val E4M3toTF32 = Wire(new RawFloat(8, 11))
        E4M3toTF32.exp := DecodeE4M3.exp.pad(8)
        E4M3toTF32.signed_sig := DecodeE4M3.signed_sig
        E4M3toTF32.exception := DecodeE4M3.exception
        E4M3toTF32.sign := DecodeE4M3.sign
        io.out(i) := E4M3toTF32
    }
}

class FVecDecoder(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(UInt(ReduceWidth.W))
        val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))
        val out = Output(new FDecodeResult)
    })

    val TF32Zero = RawFloat.fromUInt(0.U(21.W), 10, 11)

    val TF32i8 = Wire(Vec(ReduceWidth/8, new RawFloat(10, 11)))
    for(i <- 0 until ReduceWidth/8){
        TF32i8(i) := RawFloat.fromUInt(0.U(21.W), 10, 11)
        TF32i8(i).signed_sig := Mux(io.opcode === FReduceComputeType.I8I8I32,
            io.in(8 * i + 7, 8 * i).asSInt.pad(12), io.in(8 * i + 7, 8 * i).pad(12).asSInt
        )
    }

    val isF16 = io.opcode === FReduceComputeType.F16F16F32
    val isBF16 = io.opcode === FReduceComputeType.BF16BF16F32
    val isTF32 = io.opcode === FReduceComputeType.TF32TF32F32
    val isE4M3 = io.opcode === FReduceComputeType.Mxfp8e4m3F32 || io.opcode === FReduceComputeType.Fp8e4m3F32
    val isE5M2 = io.opcode === FReduceComputeType.Mxfp8e5m2F32 || io.opcode === FReduceComputeType.Fp8e5m2F32
    val isDefaultTF32Vec = !(isF16 || isBF16 || isTF32 || isE4M3 || isE5M2)
    val isDefaultTF32VecHigh = !(isE4M3 || isE5M2)

    for(i <- 0 until ReduceWidth/32){
        val Bits8  = io.in(8 * i + 7, 8 * i)
        val Bits16 = io.in(16 * i + 15, 16 * i)
        val Bits32 = io.in(32 * i + 31, 32 * i)
        val DecodeFP16 = RawFloat.fromUInt(Bits16, 5, 11)
        val DecodeBF16 = RawFloat.fromUInt(Bits16, 8, 8)
        val DecodeE4M3 = RawFloat.frome4m3(Bits8)
        val DecodeE5M2 = RawFloat.fromUInt(Bits8, 5, 3)
        val DecodeTF32 = RawFloat.fromUInt(Bits32(31, 13), 8, 11)
        val FP16toTF32 = Wire(new RawFloat(10, 11))
        val BF16toTF32 = Wire(new RawFloat(10, 11))
        val E4M3toTF32 = Wire(new RawFloat(10, 11))
        val E5M2toTF32 = Wire(new RawFloat(10, 11))
        val TF32conv = Wire(new RawFloat(10, 11))
        FP16toTF32.exp := DecodeFP16.exp.pad(10)
        FP16toTF32.signed_sig := DecodeFP16.signed_sig
        FP16toTF32.exception := DecodeFP16.exception
        FP16toTF32.sign := DecodeFP16.sign
        BF16toTF32.exp := DecodeBF16.exp
        BF16toTF32.signed_sig := Cat(DecodeBF16.signed_sig, 0.U(3.W)).asSInt
        BF16toTF32.exception := DecodeBF16.exception
        BF16toTF32.sign := DecodeBF16.sign
        E4M3toTF32.exp := DecodeE4M3.exp.pad(10)
        E4M3toTF32.signed_sig := Cat(DecodeE4M3.signed_sig, 0.U(7.W)).asSInt
        E4M3toTF32.exception := DecodeE4M3.exception
        E4M3toTF32.sign := DecodeE4M3.sign
        E5M2toTF32.exp := DecodeE5M2.exp.pad(10)
        E5M2toTF32.signed_sig := Cat(DecodeE5M2.signed_sig, 0.U(8.W)).asSInt
        E5M2toTF32.exception := DecodeE5M2.exception
        E5M2toTF32.sign := DecodeE5M2.sign
        TF32conv.exp := DecodeTF32.exp.pad(10)
        TF32conv.signed_sig := DecodeTF32.signed_sig
        TF32conv.exception := DecodeTF32.exception
        TF32conv.sign := DecodeTF32.sign
        io.out.TF32Vec(i) := Mux1H(Seq(
            isF16 -> FP16toTF32,
            isBF16 -> BF16toTF32,
            isTF32 -> TF32conv,
            isE4M3 -> E4M3toTF32,
            isE5M2 -> E5M2toTF32,
            isDefaultTF32Vec -> TF32i8(i)
        ))
    }

    for(i <- ReduceWidth/32 until ReduceWidth/16){
        val Bits8  = io.in(8 * i + 7, 8 * i)
        val Bits16 = io.in(16 * i + 15, 16 * i)
        val DecodeFP16 = RawFloat.fromUInt(Bits16, 5, 11)
        val DecodeBF16 = RawFloat.fromUInt(Bits16, 8, 8)
        val DecodeE4M3 = RawFloat.frome4m3(Bits8)
        val DecodeE5M2 = RawFloat.fromUInt(Bits8, 5, 3)
        val FP16toTF32 = Wire(new RawFloat(10, 11))
        val BF16toTF32 = Wire(new RawFloat(10, 11))
        val E4M3toTF32 = Wire(new RawFloat(10, 11))
        val E5M2toTF32 = Wire(new RawFloat(10, 11))
        FP16toTF32.exp := DecodeFP16.exp.pad(10)
        FP16toTF32.signed_sig := DecodeFP16.signed_sig
        FP16toTF32.exception := DecodeFP16.exception
        FP16toTF32.sign := DecodeFP16.sign
        BF16toTF32.exp := DecodeBF16.exp
        BF16toTF32.signed_sig := Cat(DecodeBF16.signed_sig, 0.U(3.W)).asSInt// 有符号数，所以12位
        BF16toTF32.exception := DecodeBF16.exception
        BF16toTF32.sign := DecodeBF16.sign
        E4M3toTF32.exp := DecodeE4M3.exp.pad(10)
        E4M3toTF32.signed_sig := Cat(DecodeE4M3.signed_sig, 0.U(7.W)).asSInt
        E4M3toTF32.exception := DecodeE4M3.exception
        E4M3toTF32.sign := DecodeE4M3.sign
        E5M2toTF32.exp := DecodeE5M2.exp.pad(10)
        E5M2toTF32.signed_sig := Cat(DecodeE5M2.signed_sig, 0.U(8.W)).asSInt
        E5M2toTF32.exception := DecodeE5M2.exception
        E5M2toTF32.sign := DecodeE5M2.sign
        io.out.TF32Vec(i) := Mux1H(Seq(
            isF16 -> FP16toTF32,
            isBF16 -> BF16toTF32,
            isTF32 -> TF32Zero,
            isE4M3 -> E4M3toTF32,
            isE5M2 -> E5M2toTF32,
            isDefaultTF32Vec -> TF32i8(i)
        ))
    }

    for(i <- ReduceWidth/16 until ReduceWidth/8){
        val Bits8  = io.in(8 * i + 7, 8 * i)
        val DecodeE4M3 = RawFloat.frome4m3(Bits8)
        val DecodeE5M2 = RawFloat.fromUInt(Bits8, 5, 3)
        val E4M3toTF32 = Wire(new RawFloat(10, 11))
        val E5M2toTF32 = Wire(new RawFloat(10, 11))
        E4M3toTF32.exp := DecodeE4M3.exp.pad(10)
        E4M3toTF32.signed_sig := Cat(DecodeE4M3.signed_sig, 0.U(7.W)).asSInt
        E4M3toTF32.exception := DecodeE4M3.exception
        E4M3toTF32.sign := DecodeE4M3.sign
        E5M2toTF32.exp := DecodeE5M2.exp.pad(10)
        E5M2toTF32.signed_sig := Cat(DecodeE5M2.signed_sig, 0.U(8.W)).asSInt
        E5M2toTF32.exception := DecodeE5M2.exception
        E5M2toTF32.sign := DecodeE5M2.sign
        io.out.TF32Vec(i) := Mux1H(Seq(
            isE4M3 -> E4M3toTF32,
            isE5M2 -> E5M2toTF32,
            isDefaultTF32VecHigh -> TF32i8(i)
        ))
    }
}

class CmpTreeP0Res(layers:Int, elemNum:Int, expWidth:Int)(implicit p: Parameters) extends CuteBundle{
    val part1   = UInt((elemNum).W)   //第6层mask（也就是mask(5)）,用于算后续的mask
    val part2   = Vec(expWidth - layers, UInt((elemNum).W)) //6层以后的数据传到下一层
    val part3   = UInt(layers.W) //前6层的结果
}

class CmpTreeP0(layers:Int, elemNum:Int, expWidth:Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val in    = Input(Vec(elemNum, UInt(expWidth.W)))
        val out   = Output(new CmpTreeP0Res(layers, elemNum, expWidth))
    })

  // 生成 in_vec_0: 9组，每组ReduceWidth/16位
    val inVec0 = Wire(Vec(expWidth, UInt((elemNum).W)))
    for (j <- 0 until expWidth) {
        inVec0(j) := Cat((0 until elemNum).reverse.map(i => io.in(i)(expWidth - 1 - j)))
    }

  // 生成 mask: 9组，每组ReduceWidth/16位
    val mask = Wire(Vec(layers, UInt((elemNum).W)))
    mask(0) := Mux(inVec0(0) === 0.U, ~inVec0(0), inVec0(0))
    for (i <- 1 until layers) {
        val prev = Wire(UInt((elemNum).W))
        val curr = Wire(UInt((elemNum).W))
        val prevandcurr = Wire(UInt((elemNum).W))
        prev := mask(i - 1)
        curr := inVec0(i)
        prevandcurr := (prev & curr)
        mask(i) := Mux(prevandcurr === 0.U, prev, prevandcurr)
    }

  // 生成 out: 9位
    io.out.part1 := mask(layers - 1)
    for(i <- 0 until (expWidth - layers)){
        io.out.part2(i) := inVec0(i + layers)
    }

    val res = Wire(Vec(layers, UInt(1.W)))
    for(i <- 0 until layers){
        val bit = Wire(UInt((elemNum).W))
        bit := mask(layers - 1) & inVec0(i)
        res(i) := Mux(bit === 0.U, 0.U, 1.U)
    }

    io.out.part3 := Cat((0 until layers).map(k => res(k)))
}

class CmpTreeP1(layers : Int, elemNum:Int, expWidth:Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val in    = Input(new CmpTreeP0Res(layers, elemNum, expWidth))
        val out   = Output(UInt(expWidth.W))
    })

    val mask = Wire(Vec(expWidth - layers, UInt((elemNum).W)))
    for (i <- 0 until (expWidth - layers)) {
        val prev = Wire(UInt((elemNum).W))
        val curr = Wire(UInt((elemNum).W))
        val prevandcurr = Wire(UInt((elemNum).W))
        if(i != 0){
            prev := mask(i - 1)
        }else{
            prev := io.in.part1
        }
        curr := io.in.part2(i)
        prevandcurr := (prev & curr)
        mask(i) := Mux(prevandcurr === 0.U, prev, prevandcurr)
    }

    val res = Wire(Vec(expWidth - layers, UInt(1.W)))
    for(i <- 0 until (expWidth - layers)){
        val bit = Wire(UInt(elemNum.W))
        bit := mask(expWidth - 1 - layers) & io.in.part2(i)
        res(i) := Mux(bit === 0.U, 0.U, 1.U)
    }

    val partres = Wire(UInt((expWidth - layers).W))
    partres := Cat((0 until (expWidth - layers)).map(k => res(k)))

  // 生成 out: 9位
    io.out := Cat(io.in.part3, partres)
}

class FPipe0Result(implicit p: Parameters) extends CuteBundle{
    val Product0 = Vec(ReduceWidth/16, SInt(23.W))
    val Product1 = Vec(ReduceWidth/16, SInt(23.W))
    val FP4ReduceRes = Option.when(cuteMatrixExtension.enableFp4withsf)(
        Vec(ReduceWidth/4/FP4P0AddNum, SInt((10 + log2Ceil(FP4P0AddNum)).W))
    )
    val CMantissa = SInt(32.W)
    val ExceptionVec = Vec(ReduceWidth/8 + 1, new RawFloatException) //每个向量的异常标志位
    val MulExpVec = Vec(ReduceWidth/8 + 1, UInt(10.W))
    val CmpTreefp8P0Result = new CmpTreeP0Res(fp8cmptreelayers, ReduceWidth/8 + 1, 10)
    val SignVec = Vec(ReduceWidth/8 + 1, Bool()) //每个向量的符号位
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val scaleFP = Option.when(cuteMatrixExtension.enableFp4withsf)(
        Vec(ReduceWidth/4/MinGroupSize + 1, new RawFloat(9, 11))
    )
}

class FPipe1Result(implicit p: Parameters) extends CuteBundle{
    val Product0 = Vec(ReduceWidth/16, SInt(23.W))
    val Product1 = Vec(ReduceWidth/16, SInt(23.W))
    val CMantissa = SInt(32.W)
    val RightShiftVec = Vec((ReduceWidth/8) + 1, UInt(10.W))
    val FP4ReduceRes = Option.when(cuteMatrixExtension.enableFp4withsf)(
      Vec(ReduceWidth/4/16, SInt((10 + log2Ceil(16)).W))
    )
    val MaxExp = UInt(10.W)
    val CmpTreefp4P0Result = Option.when(cuteMatrixExtension.enableFp4withsf)(
      new CmpTreeP0Res(8, ReduceWidth/4/MinGroupSize + 1, 9)
    )
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
    val SignVec = Vec(ReduceWidth/8 + 1, Bool()) //每个向量的符号位
    val scaleFP = Option.when(cuteMatrixExtension.enableFp4withsf)(
      Vec(ReduceWidth/4/MinGroupSize + 1, new RawFloat(9, 11))
    )
}

class FPipe2Result(implicit p: Parameters) extends CuteBundle{
    val ReduceRes0 = Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W))
    val ReduceRes1 = Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W)) // ReduceRes0是尾数右移后的结果，ReduceRes1是Int8的结果
    val CMantissa = SInt(32.W) // C的尾数
    val MaxExp = UInt(10.W)
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
    val FP4ABShift = Option.when(cuteMatrixExtension.enableFp4withsf)(
      Vec(ReduceWidth/4/MinGroupSize + 1, SInt((32 + 3).W))
    )
}

class FPipe3Result(implicit p: Parameters) extends CuteBundle{
    // val ReduceRes = SInt(32.W) //归约计算结果
    val ReduceRes0 = SInt((32).W)
    val ReduceRes1 = SInt((32).W) // ReduceRes0是尾数右移后的结果，ReduceRes1是Int8的结果
    // val ReduceResFP4 = SInt(32.W)
    val CMantissa = SInt(32.W) // C的尾数
    val MaxExp = UInt(10.W)
    val opcode = UInt(FReduceComputeType.ComputeTypeBitWidth.W)
    val SumException = UInt(4.W) //归约计算结果的异常标志位
}

// Stage 0:
// - Compute the product of the mantissas and the sum of the exponents
// - Find the maximum exponent and calculate the right shift amount
class FReduceMACPipe0(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val inA = Input(new FDecodeResult)
        val inB = Input(new FDecodeResult)
        val inAFP4Vec = Option.when(cuteMatrixExtension.enableFp4withsf)(
            Input(Vec(ReduceWidth/4, SInt(5.W)))
        )
        val inBFP4Vec = Option.when(cuteMatrixExtension.enableFp4withsf)(
            Input(Vec(ReduceWidth/4, SInt(5.W)))
        )
        val inAscale = Option.when(cuteMatrixExtension.enableScalingFactor)(
            Input(Vec(ReduceWidth/4/MinGroupSize, UInt(8.W)))
        )
        val inBscale = Option.when(cuteMatrixExtension.enableScalingFactor)(
            Input(Vec(ReduceWidth/4/MinGroupSize, UInt(8.W)))
        )
        val inC = Input(UInt(32.W))
        val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))
        val out = Output(new FPipe0Result)
    })

    val CDecode = RawFloat.fromUInt(io.inC, 8, 24)

    // 1. Mantissa product
    if (DEBUG_FP8) {
        for (i <- 0 until ReduceWidth/8) {
            printf("inAexp[%d]: %x\n", i.U,io.inA.TF32Vec(i).exp)
            printf("inBexp[%d]: %x\n", i.U,io.inB.TF32Vec(i).exp)
        }
    }

    // Product0
    // The width of the multiplier needs to support the multiplication of 12-bit signed integers
    for(i <- 0 until ReduceWidth/16){
        val Product0A = Wire(SInt(12.W))
        val Product0B = Wire(SInt(12.W))

        Product0A := io.inA.TF32Vec(i).signed_sig
        Product0B := io.inB.TF32Vec(i).signed_sig

        io.out.Product0(i) := Product0A * Product0B
        if (DEBUG_FP8)
        {printf("Product0A[%d]: %x\n", i.U,Product0A)
        printf("Product0B[%d]: %x\n", i.U,Product0B)
        printf("product0[%d]: %x\n", i.U,io.out.Product0(i))}
    }

    // Product1
    // The width of the multiplier needs to support the multiplication of 9-bit signed integers
    for(i <- ReduceWidth/16 until ReduceWidth/8){
        val Product1A = Wire(SInt(12.W))
        val Product1B = Wire(SInt(12.W))

        Product1A := io.inA.TF32Vec(i).signed_sig.asSInt
        Product1B := io.inB.TF32Vec(i).signed_sig.asSInt

        io.out.Product1(i - ReduceWidth/16) := Product1A * Product1B
    }

    // 2. Exponent calculation
    val MulExpVec = Wire(Vec((ReduceWidth/8) + 1, UInt(10.W)))
    val MulExpVecSigned = Wire(Vec((ReduceWidth/8) + 1, SInt(9.W))) //for debug
         
    for(i <- 0 until ReduceWidth/8){

        // printf("inA exp[%d]: %x\n", i.U,io.inA.TF32Vec(i).exp(5, 0))
        // printf("inB exp[%d]: %x\n", i.U,io.inB.TF32Vec(i).exp(5, 0))
        // printf("fp8MulExpVec[%d]: %x\n", i.U,MulExpVec(i))
        MulExpVecSigned(i) := io.inA.TF32Vec(i).exp.pad(9) + io.inB.TF32Vec(i).exp.pad(9)
        MulExpVec(i) := Mux(
            io.inA.TF32Vec(i).exception.is_zero | io.inB.TF32Vec(i).exception.is_zero,
            0.U, 
            (io.inA.TF32Vec(i).exp + io.inB.TF32Vec(i).exp).asUInt + 511.U(10.W))
        if (DEBUG_FP8) {
          printf("MulExpVec[%d]: %x\n", i.U,MulExpVec(i))
        }
    }

    MulExpVec(ReduceWidth/8) := CDecode.exp.pad(10).asUInt + 511.U(10.W) // C的阶码加上偏移量

    if (DEBUG_FP8) {
        printf("io.inC: %x\n", io.inC)
        printf("CDecode exp: %x\n", CDecode.exp.pad(10))
        printf("C exp: %x\n", MulExpVec(ReduceWidth/8))
    }

    // Each product exponent's difference with the maximum exponent is the right shift amount
    for(i <- 0 until ReduceWidth/8 + 1){
        io.out.MulExpVec(i) := MulExpVec(i)
    }

    // 3. Select max exponent
    val cmptreefp8p0 = Module(new CmpTreeP0(fp8cmptreelayers, ReduceWidth/8 + 1, 10))
    
    for (i <- 0 until ReduceWidth/16){
        cmptreefp8p0.io.in(i) := MulExpVec(i)
        cmptreefp8p0.io.in(i + ReduceWidth/16) := Mux(
            (io.opcode === FReduceComputeType.Mxfp8e4m3F32 || io.opcode === FReduceComputeType.Mxfp8e5m2F32 ||
             io.opcode === FReduceComputeType.Fp8e4m3F32 || io.opcode === FReduceComputeType.Fp8e5m2F32),
            MulExpVec(i + ReduceWidth/16), 0.U
        )
    }

    cmptreefp8p0.io.in(ReduceWidth/8) := MulExpVec(ReduceWidth/8) //最后一个C的阶码

    // Output opcode and max exponent for later use
    io.out.opcode := io.opcode
    io.out.CmpTreefp8P0Result := cmptreefp8p0.io.out

    // FP4 path
    val FP4AScaleDecoder = Option.when(cuteMatrixExtension.enableFp4withsf)(Module(new FPScaleDecoder()))
    val FP4BScaleDecoder = Option.when(cuteMatrixExtension.enableFp4withsf)(Module(new FPScaleDecoder()))
    val mxfp4ScaleSum = Option.when(cuteMatrixExtension.enableFp4withsf)(
        Wire(Vec(ReduceWidth/4/32, UInt(10.W)))
    )
    val mxfp4ScaleSat = Option.when(cuteMatrixExtension.enableFp4withsf)(
        Wire(Vec(ReduceWidth/4/32, UInt(9.W)))
    )
    val FP4AScaleExceptionVec = Option.when(cuteMatrixExtension.enableFp4withsf)(Wire(Vec(ReduceWidth/4/MinGroupSize, new RawFloatException)))
    val FP4BScaleExceptionVec = Option.when(cuteMatrixExtension.enableFp4withsf)(Wire(Vec(ReduceWidth/4/MinGroupSize, new RawFloatException)))

    if (cuteMatrixExtension.enableFp4withsf) {
        // 1. FP4 scale product
        FP4AScaleDecoder.zip(io.inAscale).foreach{
            case (decoder, scale) =>
                decoder.io.in := scale
        }
        FP4BScaleDecoder.zip(io.inBscale).foreach{
            case (decoder, scale) =>
                decoder.io.in := scale
        }

        // Results exceeding 127 will be treated as infinity.
        // So we can directly truncate to 9 bits, and results exceeding 9 bits will also be treated as infinity
        for (i <- 0 until ReduceWidth/4/32){
            mxfp4ScaleSum.get(i) := io.inAscale.get(i).asUInt.pad(10) + io.inBscale.get(i).asUInt.pad(10) + (1 + 6).U
            mxfp4ScaleSat.get(i) := Mux(mxfp4ScaleSum.get(i)(9) === 1.U, 511.U, mxfp4ScaleSum.get(i)(8, 0))
        }

        // 2. Generate exception flags for FP4 scale
        // Case Mxfp4
        val mxfp4AScaleExceptionVec = Option.when(cuteMatrixExtension.enableMxfp4Fp32)(Wire(Vec(ReduceWidth/4/32, new RawFloatException)))
        val mxfp4BScaleExceptionVec = Option.when(cuteMatrixExtension.enableMxfp4Fp32)(Wire(Vec(ReduceWidth/4/32, new RawFloatException)))
        mxfp4AScaleExceptionVec.foreach(_ := 0.U.asTypeOf(Vec(ReduceWidth/4/32, new RawFloatException)))
        mxfp4BScaleExceptionVec.foreach(_ := 0.U.asTypeOf(Vec(ReduceWidth/4/32, new RawFloatException)))
        if (cuteMatrixExtension.enableMxfp4Fp32) {
            // The format of scaling factor in mxfp4 is UE8M0
            // - 255 represents nan
            for (i <- 0 until ReduceWidth/4/32){
                mxfp4AScaleExceptionVec.get(i).is_nan := io.inAscale.get(i) === 255.U
                mxfp4BScaleExceptionVec.get(i).is_nan := io.inBscale.get(i) === 255.U
            }
        }

        // Number of blocks in mxfp4 is half of that in nvfp4
        for (i <- 0 until ReduceWidth/4/MinGroupSize){
            FP4AScaleExceptionVec.get(i) := Mux(
                io.opcode === FReduceComputeType.Mxfp4F32,
                mxfp4AScaleExceptionVec.get(i / 2),
                FP4AScaleDecoder.get.io.out(i).exception
            )
            FP4BScaleExceptionVec.get(i) := Mux(
                io.opcode === FReduceComputeType.Mxfp4F32,
                mxfp4BScaleExceptionVec.get(i / 2),
                FP4BScaleDecoder.get.io.out(i).exception
            )
        }

        // 3. Generate the scale factor in FP
        io.out.scaleFP.get(ReduceWidth/4/MinGroupSize).sign := CDecode.sign
        io.out.scaleFP.get(ReduceWidth/4/MinGroupSize).exp := CDecode.exp.pad(9) + 255.S
        if (DEBUG_FP4) {
            printf("CDecode.exp:%x\n", CDecode.exp)
            printf("io.out.scaleFP(ReduceWidth/4/MinGroupSize).exp:%x\n", io.out.scaleFP.get(ReduceWidth/4/MinGroupSize).exp)
        }
        io.out.scaleFP.get(ReduceWidth/4/MinGroupSize).signed_sig := CDecode.signed_sig
        io.out.scaleFP.get(ReduceWidth/4/MinGroupSize).exception := CDecode.exception

        for (i <- 0 until ReduceWidth/4/MinGroupSize) {
            if (DEBUG_FP4) {
                printf("io.inAscale[%d]: %x\n", i.U, io.inAscale.get(i))
                printf("io.inBscale[%d]: %x\n", i.U, io.inBscale.get(i))
                printf("FP4AScaleDecoder.io.out[%d].sign: %x\n", i.U, FP4AScaleDecoder.get.io.out(i).sign)
                printf("FP4BScaleDecoder.io.out[%d].sign: %x\n", i.U, FP4BScaleDecoder.get.io.out(i).sign)
                printf("FP4AScaleDecoder.io.out[%d].exp: %x\n", i.U, FP4AScaleDecoder.get.io.out(i).exp)
                printf("FP4BScaleDecoder.io.out[%d].exp: %x\n", i.U, FP4BScaleDecoder.get.io.out(i).exp)
            }
            io.out.scaleFP.get(i).sign := Mux(
                io.opcode === FReduceComputeType.Nvfp4F32,
                FP4AScaleDecoder.get.io.out(i).sign ^ FP4BScaleDecoder.get.io.out(i).sign,
                0.B
            )
            io.out.scaleFP.get(i).exp := Mux(
                io.out.ExceptionVec(i).is_zero || (io.opcode =/= FReduceComputeType.Nvfp4F32 && io.opcode =/= FReduceComputeType.Mxfp4F32),
                0.S, /*这里的9对应于之后对齐前的左移6，两者合起来15与FP32的尾数对齐，之所以避让出9位空位是因为FP4直接转换成整数计算中间结果位宽很大*/
                Mux(io.opcode === FReduceComputeType.Mxfp4F32,
                    mxfp4ScaleSat.get(i / 2).asSInt,
                    FP4AScaleDecoder.get.io.out(i).exp.pad(9) + FP4BScaleDecoder.get.io.out(i).exp.pad(9) + (255 + 9).S
                )
            )

            if (DEBUG_FP8 || DEBUG_FP4) {
                printf("io.out.scaleFP(%d).sign:%x\n", i.U,io.out.scaleFP.get(i).sign)
                printf("io.out.scaleFP(%d).exp:%x\n", i.U,io.out.scaleFP.get(i).exp)
            }

            val sig_mul = Wire(SInt(10.W))
            sig_mul := Mux(
                io.opcode === FReduceComputeType.Nvfp4F32,
                (FP4AScaleDecoder.get.io.out(i).signed_sig(4, 0).asSInt * FP4BScaleDecoder.get.io.out(i).signed_sig(4, 0).asSInt).asSInt,
                Mux(io.opcode === FReduceComputeType.Mxfp4F32, 1.S, 0.S)
            )
            io.out.scaleFP.get(i).signed_sig := Mux(io.out.scaleFP.get(i).sign, 
                -sig_mul.asSInt, sig_mul.asSInt
            ) 
            if (DEBUG_FP4 && cuteMatrixExtension.enableFp4withsf) {
                printf("io.out.scaleFP[%d].sign: %x\n", i.U, io.out.scaleFP.get(i).sign)
                printf("sig_mul[%d]: %x\n", i.U, sig_mul)
                printf("io.out.scaleFP[%d].signed_sig: %x\n", i.U, io.out.scaleFP.get(i).signed_sig)
            }

            io.out.scaleFP.get(i).exception := 0.U.asTypeOf(new RawFloatException)
        }

        // 4. FP4Product
        val FP4Product = Wire(Vec(ReduceWidth/4, SInt(10.W)))
        for (i <- 0 until ReduceWidth/4){
            FP4Product(i) := io.inAFP4Vec.get(i) * io.inBFP4Vec.get(i)
        }

        // 5. First level reduction
        for(i <- 0 until ReduceWidth/4/FP4P0AddNum){ 
            io.out.FP4ReduceRes.get(i) := FP4Product.slice(i * FP4P0AddNum, (i + 1) * FP4P0AddNum).reduce(_ + _)
        }
    }

    val isFp4WithScaleOp = io.opcode === FReduceComputeType.Nvfp4F32 || io.opcode === FReduceComputeType.Mxfp4F32
    for(i <- 0 until ReduceWidth/8){
        val AException = Wire(new RawFloatException)
        val BException = Wire(new RawFloatException)
        AException := io.inA.TF32Vec(i).exception
        BException := io.inB.TF32Vec(i).exception
        if (i < ReduceWidth/4/MinGroupSize) {
            FP4AScaleExceptionVec.zip(FP4BScaleExceptionVec).foreach {
                case (aScaleExceptionVec, bScaleExceptionVec) =>
                    AException := Mux(isFp4WithScaleOp, aScaleExceptionVec(i), AException)
                    BException := Mux(isFp4WithScaleOp, bScaleExceptionVec(i), BException)
            }
        }

        io.out.ExceptionVec(i).is_nan := AException.is_nan || BException.is_nan ||
            AException.is_inf && BException.is_zero ||
            BException.is_inf && AException.is_zero

        io.out.ExceptionVec(i).is_zero := AException.is_zero && !BException.is_inf ||
            BException.is_zero && !AException.is_inf

        io.out.ExceptionVec(i).is_inf := !AException.is_zero && BException.is_inf ||
            !BException.is_zero && AException.is_inf

        io.out.SignVec(i) := io.inA.TF32Vec(i).sign ^ io.inB.TF32Vec(i).sign
    }

    io.out.SignVec(ReduceWidth/8) := CDecode.sign
    MulExpVecSigned(ReduceWidth/8) := CDecode.exp.pad(9) //for debug

    io.out.ExceptionVec(ReduceWidth/8) := CDecode.exception
    io.out.CMantissa := Mux(
        (io.opcode === FReduceComputeType.F16F16F32 || io.opcode === FReduceComputeType.BF16BF16F32 ||
         io.opcode === FReduceComputeType.TF32TF32F32 || io.opcode === FReduceComputeType.Mxfp8e4m3F32 ||
         io.opcode === FReduceComputeType.Mxfp8e5m2F32 || io.opcode === FReduceComputeType.Nvfp4F32 ||
         io.opcode === FReduceComputeType.Mxfp4F32 || io.opcode === FReduceComputeType.Fp8e4m3F32 ||
         io.opcode === FReduceComputeType.Fp8e5m2F32),
        CDecode.signed_sig.pad(32), io.inC.asSInt
    )
    
    if (DEBUG_FP8) {
        printf("c mantissa: %x\n", io.out.CMantissa)
    }
}

// Pipe1的功能是将Pipe0的乘积向量尾数右移，得到尾数向量用于归约计算
class FReduceMACPipe1(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(new FPipe0Result)
        val out = Output(new FPipe1Result)
    })

    // Main path
    // 1. Continue to find the maximum exponent
    val cmptreefp8p1 = Module(new CmpTreeP1(fp8cmptreelayers, ReduceWidth/8 + 1, 10))
    cmptreefp8p1.io.in := io.in.CmpTreefp8P0Result

    if (DEBUG_FP8)
    {printf("cmptreefp8p1.io.out: %x\n", cmptreefp8p1.io.out)}

    val productMaxExp = Wire(UInt(10.W))
    productMaxExp := cmptreefp8p1.io.out

    for(i <- 0 until ReduceWidth/8){
        io.out.RightShiftVec(i) := productMaxExp - io.in.MulExpVec(i)
        if (DEBUG_FP8)
        {printf("RightShiftVec[%d]: %x\n", i.U, io.out.RightShiftVec(i) )}
    }
    io.out.RightShiftVec(ReduceWidth/8) := productMaxExp - io.in.MulExpVec(ReduceWidth/8)

    // 2. Process exceptions
    // the next stage can determine the result based on HasNaN, HasPInf, HasNInf, OnlyNZero
    val NaNVec = Wire(UInt((ReduceWidth/8 + 1).W))
    val PInfVec = Wire(UInt((ReduceWidth/8 + 1).W))
    val NInfVec = Wire(UInt((ReduceWidth/8 + 1).W))
    val NZeroVec = Wire(UInt((ReduceWidth/8 + 1).W))

    NaNVec := Cat((0 until (ReduceWidth/8 + 1)).map(i => io.in.ExceptionVec(i).is_nan))
    PInfVec := Cat((0 until (ReduceWidth/8 + 1)).map(i => io.in.ExceptionVec(i).is_inf && !io.in.SignVec(i)))
    NInfVec := Cat((0 until (ReduceWidth/8 + 1)).map(i => io.in.ExceptionVec(i).is_inf && io.in.SignVec(i)))
    NZeroVec := Cat((0 until (ReduceWidth/8 + 1)).map(i => io.in.ExceptionVec(i).is_zero && io.in.SignVec(i)))

    val HasNaN = NaNVec.orR
    val HasPInf = PInfVec.orR
    val HasNInf = NInfVec.orR
    val OnlyNZero = NZeroVec.andR

    io.out.SumException := Cat(HasNaN, HasPInf, HasNInf, OnlyNZero)

    // FP4 Path
    if (cuteMatrixExtension.enableFp4withsf) {
        val cmptreefp4p0 = Module(new CmpTreeP0(8, ReduceWidth/4/MinGroupSize + 1, 9))
        val ExpVec = Wire(Vec(ReduceWidth/4/MinGroupSize + 1, SInt(9.W)))
        for (i <- 0 until ReduceWidth/4/MinGroupSize + 1){
            ExpVec(i) := io.in.scaleFP.get(i).exp
            if (DEBUG_FP4 && cuteMatrixExtension.enableFp4withsf) {
                printf("scaleFP[%d].exp: %x\n", i.U, io.in.scaleFP.get(i).exp)
            }
        }
        cmptreefp4p0.io.in := ExpVec.map(x => x.asUInt)
        io.out.CmpTreefp4P0Result.foreach(_ := cmptreefp4p0.io.out)

        // FP4Reduce sum
        for (i <- 0 until ReduceWidth/4/16){
            io.out.FP4ReduceRes.get(i) := io.in.FP4ReduceRes.get.slice(i * FP4P1AddNum, (i + 1) * FP4P1AddNum).reduce(_ + _)
            if (DEBUG_FP4) {
                printf("FP4ReduceRes[%d]: %x\n", i.U, io.out.FP4ReduceRes.get(i))
            }
        }

        io.out.scaleFP.zip(io.in.scaleFP).foreach{ case (out, in) => out := in }
    }

    io.out.Product0 := io.in.Product0
    io.out.Product1 := io.in.Product1
    io.out.CMantissa := io.in.CMantissa
    io.out.MaxExp := productMaxExp
    io.out.opcode := io.in.opcode
    io.out.SignVec := io.in.SignVec
}

class FReduceMACPipe2(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(new FPipe1Result)
        val out = Output(new FPipe2Result)
    })

    // Main path
    val Product1 = Wire(Vec(ReduceWidth/16, SInt(26.W)))
    val Product0 = Wire(Vec(ReduceWidth/16 + 1, SInt(26.W))) //每个向量的尾数右移结果

    val ProductRShift = Wire(Vec(ReduceWidth/8 + 1, SInt(26.W)))

    ProductRShift(ReduceWidth/8) := io.in.CMantissa(25, 0).asSInt >> io.in.RightShiftVec(ReduceWidth/8)

    //这里的问题是，如果c是负数，c为补码表示，右移完之后会变成全F，取负再相加后会变成1.与右移完之后变成0的情况不符
    if (DEBUG_FP8) {
        printf("ProductRShift_C: %x\n", ProductRShift(ReduceWidth/16))
    }
    
    for(i <- 0 until ReduceWidth/16){
        val tempProduct0 = Wire(SInt(26.W))
        tempProduct0 := Cat(io.in.Product0(i)(22, 0), 0.U(3.W)).asSInt
        ProductRShift(i) := tempProduct0 >> io.in.RightShiftVec(i)
        if (DEBUG_FP8) {
            printf("ProductRShift[%d]: %x\n", i.U,ProductRShift(i))
        }
    }

    for(i <- 0 until ReduceWidth/16){
        val tempProduct1 = Wire(SInt(26.W))
        tempProduct1 := Cat(io.in.Product1(i)(22, 0), 0.U(3.W)).asSInt
        ProductRShift(ReduceWidth/16 + i) := tempProduct1 >> io.in.RightShiftVec(ReduceWidth/16 + i)
        if (DEBUG_FP8) {
            printf("ProductRShift[%d]: %x\n", (i + ReduceWidth/16).U,ProductRShift(ReduceWidth/16 + i))
        }
    }

    for(i <- 0 until ReduceWidth/16 + 1){
        if(i != ReduceWidth/16){
            Product0(i) := Mux(io.in.opcode === FReduceComputeType.I8I8I32 || io.in.opcode === FReduceComputeType.I8U8I32 || io.in.opcode === FReduceComputeType.U8I8I32 || io.in.opcode === FReduceComputeType.U8U8I32,
                io.in.Product0(i).pad(26), ProductRShift(i))
        } else {
            Product0(i) := Mux(io.in.opcode === FReduceComputeType.I8I8I32 || io.in.opcode === FReduceComputeType.I8U8I32 || io.in.opcode === FReduceComputeType.U8I8I32 || io.in.opcode === FReduceComputeType.U8U8I32,
                0.S(26.W), ProductRShift(ReduceWidth/8))
        }
    }

    for (i <- 0 until ReduceWidth/16){
        Product1(i) := Mux(io.in.opcode === FReduceComputeType.Mxfp8e4m3F32 || io.in.opcode === FReduceComputeType.Mxfp8e5m2F32 || io.in.opcode === FReduceComputeType.Fp8e4m3F32 || io.in.opcode === FReduceComputeType.Fp8e5m2F32, ProductRShift(ReduceWidth/16 + i), io.in.Product1(i))
    }

    val ProductRShiftF = Wire(Vec(ReduceWidth/16, SInt((26 + log2Ceil(P2AddNum)).W)))
    val Int8Product = Wire(Vec(ReduceWidth/16, SInt((26 + log2Ceil(P2AddNum)).W)))

    for(i <- 0 until ReduceWidth/16){
        ProductRShiftF(i) := Mux(io.in.SignVec(i), -Product0(i), Product0(i)).pad(26 + log2Ceil(P2AddNum))
    }

    for(i <- 0 until ReduceWidth/16){
        Int8Product(i) := Mux(io.in.SignVec(ReduceWidth/16 + i), -Product1(i), Product1(i)).pad(26 + log2Ceil(P2AddNum))
    }

    val SumResult0 = Wire(Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W)))
    val SumResult1 = Wire(Vec(P3AddNum, SInt((26 + log2Ceil(P2AddNum)).W)))
    for(i <- 0 until P3AddNum) {
        SumResult0(i) := ProductRShiftF.slice(i * P2AddNum, (i + 1) * P2AddNum).reduce(_ + _)
        SumResult1(i) := Int8Product.slice(i * P2AddNum, (i + 1) * P2AddNum).reduce(_ + _)
    }

    // FP4 path
    val FP4MaxExp = Option.when(cuteMatrixExtension.enableFp4withsf)(Wire(UInt(9.W)))
    if (cuteMatrixExtension.enableFp4withsf) {
        // 1. Get max exp for FP4 path
        val cmptreefp4p1 = Module(new CmpTreeP1(8, ReduceWidth/4/MinGroupSize + 1, 9))
        cmptreefp4p1.io.in := io.in.CmpTreefp4P0Result.get
        FP4MaxExp.get := cmptreefp4p1.io.out


        // 2. Shift for FP4
        val FP4Shift = Wire(Vec(ReduceWidth/4/MinGroupSize + 1, UInt(9.W)))
        for (i <- 0 until ReduceWidth/4/MinGroupSize + 1){
            FP4Shift(i) := FP4MaxExp.get.asUInt - io.in.scaleFP.get(i).exp.asUInt
        }

        // 3. Block dot multiply scale mantissa, and left shift
        for (i <- 0 until ReduceWidth/4/MinGroupSize){
            // 32 bits in tempFP4
            // reserve 3b on the left to reduce the impact of -1 accumulation caused by right shift of negative numbers
            val tempFP4 = Wire(SInt((32 + 3).W))
            tempFP4 := (io.in.FP4ReduceRes.get(i) * io.in.scaleFP.get(i).signed_sig).pad(32 + 3) <<
                Mux(
                    io.in.opcode === FReduceComputeType.Mxfp4F32, (15 + 3).U, (6 + 3).U
                )
            io.out.FP4ABShift.get(i) := tempFP4 >> FP4Shift(i)
            if (DEBUG_FP4 && cuteMatrixExtension.enableFp4withsf) {
                printf("FP4Shift[%d]: %x\n", i.U, FP4Shift(i))
                printf("tempFP4[%d]: %x\n", i.U, tempFP4)
                printf("io.out.FP4ABShift[%d]: %x\n", i.U, io.out.FP4ABShift.get(i))
                printf("without pad io.out.FP4ABShift[%d]: %x\n", i.U, io.out.FP4ABShift.get(i)(34, 3))
            }
        }
        if (DEBUG_FP4 && cuteMatrixExtension.enableFp4withsf) {
            printf("FP4Shift[8]: %x\n", FP4Shift(ReduceWidth/4/MinGroupSize))
            printf("io.out.FP4ABShift[8]: %x\n", io.out.FP4ABShift.get(ReduceWidth/4/MinGroupSize))
        }

        // 4. Process CMantissa for FP4 path
        val FP4signedCMantissa = Wire(SInt((32 + 3).W))
        FP4signedCMantissa := Cat(io.in.CMantissa, 0.U(3.W)).asSInt >> FP4Shift(ReduceWidth/4/MinGroupSize)
        io.out.FP4ABShift.get(ReduceWidth/4/MinGroupSize) := Mux(
          io.in.SignVec(ReduceWidth/8), -FP4signedCMantissa, FP4signedCMantissa
        )

        if (DEBUG_FP4) {
            printf("FP4MaxExp: %x\n", FP4MaxExp.get)
        }
    }

    io.out.ReduceRes0 := SumResult0
    io.out.ReduceRes1 := SumResult1
    io.out.CMantissa := Mux(io.in.opcode === FReduceComputeType.I8I8I32 || io.in.opcode === FReduceComputeType.I8U8I32 || io.in.opcode === FReduceComputeType.U8I8I32 || io.in.opcode === FReduceComputeType.U8U8I32,
        io.in.CMantissa, Mux(io.in.SignVec(ReduceWidth/8), -Product0(ReduceWidth/16), Product0(ReduceWidth/16)).pad(32))
    // Select MaxExp
    val isFp4WithScaleOp = io.in.opcode === FReduceComputeType.Nvfp4F32 || io.in.opcode === FReduceComputeType.Mxfp4F32
    io.out.MaxExp := io.in.MaxExp
    FP4MaxExp.foreach { fp4MaxExp =>
        io.out.MaxExp := Mux(isFp4WithScaleOp, fp4MaxExp.asUInt.pad(10) + 256.U, io.in.MaxExp)
    }
    if (DEBUG_FP4) {
        printf("io.out.MaxExp: %x\n", io.out.MaxExp)
    }
    io.out.opcode := io.in.opcode
    io.out.SumException := io.in.SumException
}

// Stage 3: Final reduction
class FReduceMACPipe3(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(new FPipe2Result)
        val out = Output(new FPipe3Result)
    })

    val SumResult0 = Wire(Vec(P3AddNum, SInt((32 + 3).W)))
    val SumResult1 = Wire(Vec(P3AddNum, SInt((32).W)))
    val isFp4WithScaleOp = io.in.opcode === FReduceComputeType.Nvfp4F32 || io.in.opcode === FReduceComputeType.Mxfp4F32
    for(i <- 0 until P3AddNum) {
        val normalReduce = Cat(io.in.ReduceRes0(i).pad(32), 0.U(3.W)).asSInt
        SumResult0(i) := normalReduce
        io.in.FP4ABShift.foreach { fp4ABShift =>
            SumResult0(i) := Mux(isFp4WithScaleOp, fp4ABShift(i), normalReduce)
        }
        SumResult1(i) := io.in.ReduceRes1(i).pad(32)
    }

    val normalCAddend = Cat(io.in.CMantissa, 0.U(3.W)).asSInt
    val cAddend = Wire(SInt((32 + 3).W))
    cAddend := normalCAddend
    io.in.FP4ABShift.foreach { fp4ABShift =>
        cAddend := Mux(isFp4WithScaleOp, fp4ABShift(ReduceWidth / 4 / MinGroupSize), normalCAddend)
    }
    io.out.ReduceRes0 := (SumResult0.reduce(_ + _) + cAddend)(34, 3).asSInt
    
    io.out.ReduceRes1 := SumResult1.reduce(_ + _)

    if (DEBUG_FP8) {
        printf("CMantissa: %x\n", io.in.CMantissa)
    }

    io.out.CMantissa := io.in.CMantissa
    io.out.MaxExp := io.in.MaxExp
    io.out.opcode := io.in.opcode
    io.out.SumException := io.in.SumException
}

// Stage 4: Result selection, exception handling, normalization
class FReduceMACPipe4(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val in = Input(new FPipe3Result)
        val out = UInt(ResultWidth.W)
    })

    val ReduceResF = Wire(SInt(32.W))
    val ReduceResI = Wire(SInt(32.W))
    val ReduceRes  = Wire(SInt(32.W))
    val SumResult0 = Wire(SInt((32).W))
    val SumResult1 = Wire(SInt((32).W))
    
    SumResult0 := io.in.ReduceRes0.pad(32) //.pad(26 + log2Ceil(ReduceWidth/16))
    SumResult1 := io.in.ReduceRes1.pad(32)


    ReduceRes := Mux(
        (io.in.opcode === FReduceComputeType.I8I8I32 || io.in.opcode === FReduceComputeType.I8U8I32 ||
         io.in.opcode === FReduceComputeType.U8I8I32 || io.in.opcode === FReduceComputeType.U8U8I32 ||
         io.in.opcode === FReduceComputeType.Mxfp8e4m3F32 || io.in.opcode === FReduceComputeType.Mxfp8e5m2F32 ||
         io.in.opcode === FReduceComputeType.Fp8e4m3F32 || io.in.opcode === FReduceComputeType.Fp8e5m2F32),
        SumResult0 + SumResult1.pad(32), SumResult0
    )

    if (DEBUG_FP8 || DEBUG_FP4)
    {
        printf("SumResult0:%x\n", SumResult0)
        printf("SumResult1:%x\n", SumResult1)
        printf("ReduceRes:%x\n", ReduceRes)
    }

    ReduceResF := ReduceRes
    ReduceResI := ReduceRes

    val ExceptionBits = Wire(UInt(32.W))
    val IsException = Wire(Bool())
    IsException := io.in.SumException.orR || (ReduceRes === 0.S)

    ExceptionBits := Mux(io.in.SumException(3) || io.in.SumException(2) && io.in.SumException(1), 0x7FC00000.U,
        Mux(io.in.SumException(2), 0x7F800000.U, 
            Mux(io.in.SumException(1), BigInt("FF800000", 16).U, 0.U)
        )
    )

    //尾数符号位
    val ResultSign = Wire(Bool())
    ResultSign := ReduceResF(31)

    //去掉符号位，正直接转无符号数，负先有符号数取负再转无符号数
    val UnsignedSig = Wire(UInt(32.W))
    UnsignedSig := Mux(ResultSign, 
        (-ReduceResF.asSInt).asUInt, 
        ReduceResF.asUInt
    )
    if (DEBUG_FP8)
    {printf("UnsignedSig:%x\n", UnsignedSig)}

    val clz = Module(new CLZ(32))
    clz.io.in := UnsignedSig
    val LeadingZeros = Wire(UInt(5.W))
    LeadingZeros := clz.io.out
    if (DEBUG_FP8)
    {printf("LZ:%d\n", LeadingZeros)}
    //左移相同，该怎么左移就怎么左移,注意要把最高位的1省略掉作为规格化结果

    //修改一下这里规格化的逻辑
    //真正问题在乘法，乘法尾数是0，但阶码正常计算了
    //最终导致阶码比大小的时候，阶码较大的数尾数是0，不参与加法，而阶码第二大的数，尾数又要右移，导致丢精度，例如fp16_xtest.txt中的数据
    val ShiftMantissa = Wire(SInt(32.W))
    val ShiftExp = Wire(SInt(16.W))

    ShiftExp := (io.in.MaxExp - 511.U(10.W)).asSInt.pad(16) - LeadingZeros.pad(16).asSInt + 8.S(16.W)
    ShiftMantissa := Mux(LeadingZeros > 8.U, UnsignedSig.asSInt << (LeadingZeros - 8.U),
        UnsignedSig.asSInt >> (8.U - LeadingZeros) //这里右移导致移掉了低位的非零位，丢失信息，需要改
    )

    if (DEBUG_FP4) {
        printf("ShiftExp:%x\n", ShiftExp)
    }

    if (DEBUG_FP8 || DEBUG_FP4) {
        printf("ShiftMantissa:%x\n", ShiftMantissa)
    }

    val ResultExp = Wire(UInt(8.W))
    val ResultSig = Wire(UInt(24.W))
    ResultExp := Mux(ShiftExp < -126.S(16.W), 0.U, 
        Mux(ShiftExp > 127.S(16.W), 255.U, (ShiftExp + 127.S(16.W)).asUInt)
    )
    ResultSig := Mux(ShiftExp < -149.S(16.W), 0.U, 
        Mux(ShiftExp < -126.S(16.W), ShiftMantissa(23, 0).asUInt >> (-126.S(16.W) - ShiftExp).asUInt,
            Mux(ShiftExp > 127.S(16.W), 0.U, ShiftMantissa(23, 0).asUInt)
        )
    )

    val Result = Wire(UInt(ResultWidth.W))
    if (DEBUG_FP8 || DEBUG_FP4) {
        printf("Result:%x\n", Result)
    }
    Result := Cat(ResultSign, ResultExp, ResultSig(22, 0))
    io.out := Mux(
        (io.in.opcode === FReduceComputeType.I8I8I32 || io.in.opcode === FReduceComputeType.I8U8I32 ||
         io.in.opcode === FReduceComputeType.U8I8I32 || io.in.opcode === FReduceComputeType.U8U8I32),
        ReduceResI.asUInt, Mux(IsException, ExceptionBits, Result)
    )
}

class FReducePE(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle{
        val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
        val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
        val CAdd    = Flipped(DecoupledIO(UInt(ResultWidth.W)))
        val AScale  = Option.when(cuteMatrixExtension.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ReduceWidth/MinDataTypeWidth/MinGroupSize * ScaleElementWidth).W)))
        )
        val BScale  = Option.when(cuteMatrixExtension.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ReduceWidth/MinDataTypeWidth/MinGroupSize * ScaleElementWidth).W)))
        )
        val DResult = DecoupledIO(UInt(ResultWidth.W))
        val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))   //0:Int8, 1:FP16, 2:BF16, 3:TF32, 4:I8 * UI8, 5:UI8 * I8, 6:UI8 * UI8, 7:MXFP8E4M3, 8:MXFP8e5m2 9:NVFP4, 10:MXFP4, 11:FP8E4M3, 12:FP8E5M2
    })

    // for DEBUG
    val count = RegInit(0.U(32.W))
    count := count + 1.U

    val pipe0 = Module(new FReduceMACPipe0)
    val pipe1 = Module(new FReduceMACPipe1)
    val pipe2 = Module(new FReduceMACPipe2)
    val pipe3 = Module(new FReduceMACPipe3)
    val pipe4 = Module(new FReduceMACPipe4)

    if (DEBUG_FP8 || DEBUG_FP4) {
        printf("Cycle count: %x\n", count)

        printf("io.AVector.bits: %x\n", io.AVector.bits)
        printf("io.BVector.bits: %x\n", io.BVector.bits)
        if (cuteMatrixExtension.enableScalingFactor) {
            printf("op.AScale.bits[0]: %x\n", io.AScale.get.bits(7, 0))
            printf("op.BScale.bits[0]: %x\n", io.BScale.get.bits(7, 0))
            printf("AScale: %x\n", io.AScale.get.bits)
        }
    }


    val PipeResRegValid = RegInit(VecInit(Seq.fill(6)(false.B)))

    val InputRegC = Reg(UInt((ResultWidth + FReduceComputeType.ComputeTypeBitWidth).W))
    val InputRegA = Reg(new FDecodeResult)
    val InputRegB = Reg(new FDecodeResult)
    val InputRegAFP4Vec = Option.when(cuteMatrixExtension.enableFp4withsf)(
      Reg(Vec(ReduceWidth/4, SInt(5.W)))
    )
    val InputRegBFP4Vec = Option.when(cuteMatrixExtension.enableFp4withsf)(
      Reg(Vec(ReduceWidth/4, SInt(5.W)))
    )
    val InputRegAscale = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Reg(Vec(ReduceWidth/MinDataTypeWidth/MinGroupSize, UInt(ScaleElementWidth.W)))
    )
    val InputRegBscale = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Reg(Vec(ReduceWidth/MinDataTypeWidth/MinGroupSize, UInt(ScaleElementWidth.W)))
    )
    val Pipe0ResReg = Reg(new FPipe0Result)
    val Pipe1ResReg = Reg(new FPipe1Result)
    val Pipe2ResReg = Reg(new FPipe2Result)
    val Pipe3ResReg = Reg(new FPipe3Result)
    val Pipe4ResReg = Reg(UInt(ResultWidth.W))

    val InReady = Wire(Bool())
    val Pipe0ResRegAllowIn = Wire(Bool())
    val Pipe1ResRegAllowIn = Wire(Bool())
    val Pipe2ResRegAllowIn = Wire(Bool())
    val Pipe3ResRegAllowIn = Wire(Bool())
    val Pipe4ResRegAllowIn = Wire(Bool())

    Pipe4ResRegAllowIn := (!PipeResRegValid(5)) || io.DResult.ready
    Pipe3ResRegAllowIn := (!PipeResRegValid(4)) || Pipe4ResRegAllowIn
    Pipe2ResRegAllowIn := (!PipeResRegValid(3)) || Pipe3ResRegAllowIn
    Pipe1ResRegAllowIn := (!PipeResRegValid(2)) || Pipe2ResRegAllowIn
    Pipe0ResRegAllowIn := (!PipeResRegValid(1)) || Pipe1ResRegAllowIn
    InReady := (!PipeResRegValid(0)) || Pipe0ResRegAllowIn

    io.AVector.ready := InReady
    io.BVector.ready := InReady
    io.AScale.foreach(_.ready := InReady)
    io.BScale.foreach(_.ready := InReady)
    io.CAdd.ready := InReady

    val isScaleFactorOp =
        io.opcode === FReduceComputeType.Mxfp8e4m3F32 ||
        io.opcode === FReduceComputeType.Mxfp8e5m2F32 ||
        io.opcode === FReduceComputeType.Nvfp4F32 ||
        io.opcode === FReduceComputeType.Mxfp4F32
    val inputValid = io.AVector.valid && io.BVector.valid && io.CAdd.valid
    val scaleValid = io.AScale.zip(io.BScale).map { case (aScale, bScale) =>
        aScale.valid && bScale.valid
    }.getOrElse(false.B)
    val ABCValid = inputValid && (!isScaleFactorOp || scaleValid)

    // Stage -1 (Path A): Transform input vector to unified format
    val ADecoder = Module(new FVecDecoder)
    val BDecoder = Module(new FVecDecoder)

    ADecoder.io.in := io.AVector.bits
    ADecoder.io.opcode := Mux(
        io.opcode === FReduceComputeType.I8U8I32 || io.opcode === FReduceComputeType.I8I8I32,
        FReduceComputeType.I8I8I32,
        Mux(
            io.opcode === FReduceComputeType.U8I8I32 || io.opcode === FReduceComputeType.U8U8I32,
            FReduceComputeType.I8U8I32,
            io.opcode
        )
    )
    BDecoder.io.in := io.BVector.bits
    BDecoder.io.opcode := Mux(
        io.opcode === FReduceComputeType.U8I8I32 || io.opcode === FReduceComputeType.I8I8I32,
        FReduceComputeType.I8I8I32,
        Mux(
            io.opcode === FReduceComputeType.I8U8I32 || io.opcode === FReduceComputeType.U8U8I32,
            FReduceComputeType.I8U8I32,
            io.opcode
        )
    )

    // For MXFP8, directly add UE8M0 scaling factor to exponent.
    val scaleSum = io.AScale.zip(io.BScale).flatMap { case (aScale, bScale) =>
      Option.when(cuteMatrixExtension.enableMxfp8Fp32) {
        val sum = Wire(Vec(ReduceWidth/8/32, UInt(9.W)))
        for (i <- 0 until ReduceWidth/8/32) {
            sum(i) := aScale.bits(i * 8 + 7, i * 8).asUInt.pad(10) + bScale.bits(i * 8 + 7, i * 8).asUInt.pad(10) - 254.U
        }
        sum
      }
    }

    // Store the result of stage -1 (FVecDecoder) to registers
    when(InReady){
        when(ABCValid){
            InputRegC := Cat(io.CAdd.bits, io.opcode)
            InputRegA := ADecoder.io.out
            InputRegB := BDecoder.io.out
            val isMxfp8Op = (
              io.opcode === FReduceComputeType.Mxfp8e4m3F32 ||
              io.opcode === FReduceComputeType.Mxfp8e5m2F32
            )
            scaleSum.foreach { sum =>
                for (i <- 0 until ReduceWidth/8/32) {
                    for (j <- 0 until 32) {
                        when(isMxfp8Op) {
                            // Add UE8M0 scaling factor to Vec A's exponent.
                            InputRegA.TF32Vec(i * 32 + j).exp :=
                                sum(i).asSInt.pad(10) + ADecoder.io.out.TF32Vec(i * 32 + j).exp.pad(10)
                        }
                    }
                }
            }
            InputRegAscale.zip(io.AScale).foreach{
              case (reg, scale) => reg := scale.bits.asTypeOf(InputRegAscale.get)
            }
            InputRegBscale.zip(io.BScale).foreach{
              case (reg, scale) => reg := scale.bits.asTypeOf(InputRegBscale.get)
            }
            PipeResRegValid(0) := true.B
        }.otherwise{
            InputRegC := InputRegC
            PipeResRegValid(0) := false.B
        }
    }.otherwise{
        InputRegC := InputRegC
        PipeResRegValid(0) := PipeResRegValid(0)
    }

    // Stage -1 (Path B): From fp4 to fixed point
    if (cuteMatrixExtension.enableFp4withsf) {
        // FP4 to int
        for (i <- 0 until ReduceWidth/4){
            val fp4DecodeA = Module(new FP4toint)
            val fp4DecodeB = Module(new FP4toint)
            fp4DecodeA.io.in := io.AVector.bits((i+1)*4-1, i*4)
            fp4DecodeB.io.in := io.BVector.bits((i+1)*4-1, i*4)
            when(InReady && ABCValid && (io.opcode === FReduceComputeType.Nvfp4F32 || io.opcode === FReduceComputeType.Mxfp4F32)){
                InputRegAFP4Vec.get(i) := fp4DecodeA.io.out
                InputRegBFP4Vec.get(i) := fp4DecodeB.io.out
            }
        }
    }
    
    // Stage 0: Multiply mantissa and prepare exponent
    // Main path in stage 0
    pipe0.io.inA := InputRegA
    pipe0.io.inB := InputRegB
    pipe0.io.inC := InputRegC(ResultWidth + FReduceComputeType.ComputeTypeBitWidth - 1, FReduceComputeType.ComputeTypeBitWidth)
    pipe0.io.inAscale.zip(InputRegAscale).foreach{
      case (scale, reg) => scale := reg
    }
    pipe0.io.inBscale.zip(InputRegBscale).foreach{
      case (scale, reg) => scale := reg
    }
    // FP4 path in stage 0
    pipe0.io.inAFP4Vec.zip(InputRegAFP4Vec).foreach{
      case (vec, reg) => vec := reg
    }
    pipe0.io.inBFP4Vec.zip(InputRegBFP4Vec).foreach{
      case (vec, reg) => vec := reg
    }
    pipe0.io.opcode := InputRegC(FReduceComputeType.ComputeTypeBitWidth - 1, 0)

    when(Pipe0ResRegAllowIn){
        when(PipeResRegValid(0)){
            PipeResRegValid(1) := true.B
            Pipe0ResReg := pipe0.io.out
        }.otherwise{
            PipeResRegValid(1) := false.B
            Pipe0ResReg := Pipe0ResReg
        }
    }.otherwise{
        PipeResRegValid(1) := PipeResRegValid(1)
        Pipe0ResReg := Pipe0ResReg
    }

    pipe1.io.in := Pipe0ResReg

    when(Pipe1ResRegAllowIn){
        when(PipeResRegValid(1)){
            PipeResRegValid(2) := true.B
            Pipe1ResReg := pipe1.io.out
        }.otherwise{
            PipeResRegValid(2) := false.B
            Pipe1ResReg := Pipe1ResReg
        }
    }.otherwise{
        PipeResRegValid(2) := PipeResRegValid(2)
        Pipe1ResReg := Pipe1ResReg
    }

    pipe2.io.in := Pipe1ResReg

    when(Pipe2ResRegAllowIn){
        when(PipeResRegValid(2)){
            PipeResRegValid(3) := true.B
            Pipe2ResReg := pipe2.io.out
        }.otherwise{
            PipeResRegValid(3) := false.B
            Pipe2ResReg := Pipe2ResReg
        }
    }.otherwise{
        PipeResRegValid(3) := PipeResRegValid(3)
        Pipe2ResReg := Pipe2ResReg
    }

    pipe3.io.in := Pipe2ResReg

    when(Pipe3ResRegAllowIn){
        when(PipeResRegValid(3)){
            PipeResRegValid(4) := true.B
            Pipe3ResReg := pipe3.io.out
        }.otherwise{
            PipeResRegValid(4) := false.B
            Pipe3ResReg := Pipe3ResReg
        }
    }.otherwise{
        PipeResRegValid(4) := PipeResRegValid(4)
        Pipe3ResReg := Pipe3ResReg
    }

    pipe4.io.in := Pipe3ResReg

    when(Pipe4ResRegAllowIn){
        when(PipeResRegValid(4)){
            PipeResRegValid(5) := true.B
            Pipe4ResReg := pipe4.io.out
        }.otherwise{
            PipeResRegValid(5) := false.B
            Pipe4ResReg := Pipe4ResReg
        }
    }.otherwise{
        PipeResRegValid(5) := PipeResRegValid(5)
        Pipe4ResReg := Pipe4ResReg
    }

    io.DResult.valid := PipeResRegValid(5)
    io.DResult.bits := Pipe4ResReg
}

class top (implicit p: Parameters) extends FReducePE {}
