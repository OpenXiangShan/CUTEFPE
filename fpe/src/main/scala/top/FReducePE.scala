package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class FReducePE(implicit p: Parameters) extends CuteFpeModule {
    val io = IO(new Bundle{
        val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
        val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
        val CAdd    = Flipped(DecoupledIO(UInt(ResultWidth.W)))
        val AScale  = Option.when(cuteFpeConfig.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ReduceWidth/MinDataTypeWidth/MinGroupSize * ScaleElementWidth).W)))
        )
        val BScale  = Option.when(cuteFpeConfig.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ReduceWidth/MinDataTypeWidth/MinGroupSize * ScaleElementWidth).W)))
        )
        val DResult = DecoupledIO(UInt(ResultWidth.W))
        // Local IEEE sideband, atomic with DResult; not an architectural CSR.
        val DFlags = Output(UInt(5.W))
        
        //0:Int8, 1:FP16, 2:BF16, 3:TF32, 4:I8 * UI8, 5:UI8 * I8, 6:UI8 * UI8, 7:MXFP8E4M3, 8:MXFP8e5m2 9:NVFP4, 10:MXFP4, 11:FP8E4M3, 12:FP8E5M2
        val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))
        val LiveK = Option.when(cuteFpeConfig.enableMxfp4Fp32 || cuteFpeConfig.enableMxfp8Fp32)(
            Input(UInt(log2Ceil(ReduceWidth / 4 + 1).W)))
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
        if (cuteFpeConfig.enableScalingFactor) {
            printf("op.AScale.bits[0]: %x\n", io.AScale.get.bits(7, 0))
            printf("op.BScale.bits[0]: %x\n", io.BScale.get.bits(7, 0))
            printf("AScale: %x\n", io.AScale.get.bits)
        }
    }


    val PipeResRegValid = RegInit(VecInit(Seq.fill(6)(false.B)))

    val InputRegC = Reg(UInt((ResultWidth + FReduceComputeType.ComputeTypeBitWidth).W))
    val InputRegA = Reg(new FDecodeResult)
    val InputRegB = Reg(new FDecodeResult)
    val InputRegAFP4Vec = Option.when(cuteFpeConfig.enableFp4withsf)(
      Reg(Vec(ReduceWidth/4, SInt(5.W)))
    )
    val InputRegBFP4Vec = Option.when(cuteFpeConfig.enableFp4withsf)(
      Reg(Vec(ReduceWidth/4, SInt(5.W)))
    )
    val InputRegFP4Sign = Option.when(cuteFpeConfig.enableMxfp4Fp32)(
      Reg(Vec(ReduceWidth / 4, Bool()))
    )
    val InputRegAscale = Option.when(cuteFpeConfig.enableScalingFactor)(
      Reg(Vec(ReduceWidth/MinDataTypeWidth/MinGroupSize, UInt(ScaleElementWidth.W)))
    )
    val InputRegBscale = Option.when(cuteFpeConfig.enableScalingFactor)(
      Reg(Vec(ReduceWidth/MinDataTypeWidth/MinGroupSize, UInt(ScaleElementWidth.W)))
    )
    val InputRegLiveK = io.LiveK.map(k => Reg(UInt(k.getWidth.W)))
    val Pipe0ResReg = Reg(new FPipe0Result)
    val Pipe1ResReg = Reg(new FPipe1Result)
    val Pipe2ResReg = Reg(new FPipe2Result)
    val Pipe3ResReg = Reg(new FPipe3Result)
    val Pipe4ResReg = Reg(UInt(ResultWidth.W))
    val Pipe4FlagsReg = Reg(UInt(5.W))

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

    // Padding is not a real zero product. Mask its coefficient here, retain
    // exact live K for classification and ordered-add/flags bypass below.
    val decodeA = WireDefault(io.AVector.bits)
    val decodeB = WireDefault(io.BVector.bits)
    io.LiveK.foreach { live =>
        val fp4 = io.opcode === FReduceComputeType.Mxfp4F32
        val mx8 = io.opcode === FReduceComputeType.Mxfp8e4m3F32 ||
            io.opcode === FReduceComputeType.Mxfp8e5m2F32
        val a4 = VecInit((0 until ReduceWidth / 4).map(i =>
            Mux(i.U < live, io.AVector.bits(i * 4 + 3, i * 4), 0.U(4.W))))
        val b4 = VecInit((0 until ReduceWidth / 4).map(i =>
            Mux(i.U < live, io.BVector.bits(i * 4 + 3, i * 4), 0.U(4.W))))
        val a8 = VecInit((0 until ReduceWidth / 8).map(i =>
            Mux(i.U < live, io.AVector.bits(i * 8 + 7, i * 8), 0.U(8.W))))
        val b8 = VecInit((0 until ReduceWidth / 8).map(i =>
            Mux(i.U < live, io.BVector.bits(i * 8 + 7, i * 8), 0.U(8.W))))
        when(fp4) { decodeA := a4.asUInt; decodeB := b4.asUInt }
        when(mx8) { decodeA := a8.asUInt; decodeB := b8.asUInt }
        when(InReady && ABCValid && (fp4 || mx8)) {
            assert(live <= Mux(fp4, (ReduceWidth / 4).U, (ReduceWidth / 8).U))
        }
    }
    ADecoder.io.in := decodeA
    ADecoder.io.opcode := Mux(
        io.opcode === FReduceComputeType.I8U8I32 || io.opcode === FReduceComputeType.I8I8I32,
        FReduceComputeType.I8I8I32,
        Mux(
            io.opcode === FReduceComputeType.U8I8I32 || io.opcode === FReduceComputeType.U8U8I32,
            FReduceComputeType.I8U8I32,
            io.opcode
        )
    )
    BDecoder.io.in := decodeB
    BDecoder.io.opcode := Mux(
        io.opcode === FReduceComputeType.U8I8I32 || io.opcode === FReduceComputeType.I8I8I32,
        FReduceComputeType.I8I8I32,
        Mux(
            io.opcode === FReduceComputeType.I8U8I32 || io.opcode === FReduceComputeType.U8U8I32,
            FReduceComputeType.I8U8I32,
            io.opcode
        )
    )

    // Store the result of stage -1 (FVecDecoder) to registers
    when(InReady){
        when(ABCValid){
            InputRegC := Cat(io.CAdd.bits, io.opcode)
            InputRegA := ADecoder.io.out
            InputRegB := BDecoder.io.out
            InputRegLiveK.zip(io.LiveK).foreach { case (reg, live) => reg := live }
            // Keep the unscaled decoded exponents exact. MX's common scale
            // is carried separately until after each block's exact reduction.
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
    InputRegFP4Sign.foreach { signs =>
        when(InReady && ABCValid) {
            for (i <- 0 until ReduceWidth / 4) {
                // FP4toint necessarily loses -0; retain the product sign here.
                signs(i) := io.AVector.bits(i * 4 + 3) ^ io.BVector.bits(i * 4 + 3)
            }
        }
    }
    if (cuteFpeConfig.enableFp4withsf) {
        // FP4 to int
        for (i <- 0 until ReduceWidth/4){
            val fp4DecodeA = Module(new FP4toint)
            val fp4DecodeB = Module(new FP4toint)
            fp4DecodeA.io.in := decodeA((i+1)*4-1, i*4)
            fp4DecodeB.io.in := decodeB((i+1)*4-1, i*4)
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
    pipe0.io.inFP4Sign.zip(InputRegFP4Sign).foreach { case (port, reg) => port := reg }
    pipe0.io.inLiveK.zip(InputRegLiveK).foreach { case (port, reg) => port := reg }
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
            Pipe4FlagsReg := pipe4.io.flags
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
    io.DFlags := Pipe4FlagsReg
}

class top (implicit p: Parameters) extends FReducePE {}
