package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

/**
  * Internal FPE datapath. The external FReducePE wrapper retains the legacy
  * four-bit MatrixTE contract while this module carries the complete opcode
  * namespace used by the pipeline.
  */
class FReduceCore(implicit p: Parameters) extends CuteFpeModule {
  private val scaleBusWidth = ReduceWidth / MinDataTypeWidth / MinGroupSize * ScaleElementWidth

  require(ReduceWidth == 256 || ReduceWidth == 512,
    s"CUTEFPE supports ReduceWidth=256 or 512, got $ReduceWidth")
  require(ResultWidth == 32, s"CUTEFPE requires ResultWidth=32, got $ResultWidth")

  val io = IO(new Bundle {
    val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
    val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
    val CAdd = Flipped(DecoupledIO(UInt(ResultWidth.W)))
    val AScale = Flipped(DecoupledIO(UInt(scaleBusWidth.W)))
    val BScale = Flipped(DecoupledIO(UInt(scaleBusWidth.W)))
    val opcode = Input(UInt(FpeOpcode.Width.W))
    val DResult = DecoupledIO(UInt(ResultWidth.W))
  })

  // Each module is the combinational logic between two named pipeline cuts.
  val pipe0 = Module(new FReduceMACPipe0)
  val pipe1 = Module(new FReduceMACPipe1)
  val pipe2 = Module(new FReduceMACPipe2)
  val pipe3 = Module(new FReduceMACPipe3)
  val pipe4 = Module(new FReduceMACPipe4)
  val pipe5 = Module(new FReduceMACPipe5)
  val pipe6 = Module(new FReduceMACPipe6)
  val pipe7 = Module(new FReduceMACPipe7)
  val pipe8 = Module(new FReduceMACPipe8)

  val PipeResRegValid = RegInit(VecInit(Seq.fill(9)(false.B)))
  val Pipe0ResReg = Reg(new FPipe0Result)
  val Pipe1ResReg = Reg(new FPipe1Result)
  val Pipe2ResReg = Reg(new FPipe2Result)
  val Pipe3ResReg = Reg(new FPipe3Result)
  val Pipe4ResReg = Reg(new FPipe4Result)
  val Pipe5ResReg = Reg(new FPipe5Result)
  val Pipe6ResReg = Reg(new FPipe6Result)
  val Pipe7ResReg = Reg(new FPipe7Result)
  val Pipe8ResReg = Reg(new FPipe8Result)

  val InReady = Wire(Bool())
  val Pipe1ResRegAllowIn = Wire(Bool())
  val Pipe2ResRegAllowIn = Wire(Bool())
  val Pipe3ResRegAllowIn = Wire(Bool())
  val Pipe4ResRegAllowIn = Wire(Bool())
  val Pipe5ResRegAllowIn = Wire(Bool())
  val Pipe6ResRegAllowIn = Wire(Bool())
  val Pipe7ResRegAllowIn = Wire(Bool())
  val Pipe8ResRegAllowIn = Wire(Bool())

  Pipe8ResRegAllowIn := !PipeResRegValid(8) || io.DResult.ready
  Pipe7ResRegAllowIn := !PipeResRegValid(7) || Pipe8ResRegAllowIn
  Pipe6ResRegAllowIn := !PipeResRegValid(6) || Pipe7ResRegAllowIn
  Pipe5ResRegAllowIn := !PipeResRegValid(5) || Pipe6ResRegAllowIn
  Pipe4ResRegAllowIn := !PipeResRegValid(4) || Pipe5ResRegAllowIn
  Pipe3ResRegAllowIn := !PipeResRegValid(3) || Pipe4ResRegAllowIn
  Pipe2ResRegAllowIn := !PipeResRegValid(2) || Pipe3ResRegAllowIn
  Pipe1ResRegAllowIn := !PipeResRegValid(1) || Pipe2ResRegAllowIn
  InReady := !PipeResRegValid(0) || Pipe1ResRegAllowIn

  val scaleRequired = FpeOpcode.requiresScale(io.opcode)
  val inputValid = io.AVector.valid && io.BVector.valid && io.CAdd.valid &&
    (!scaleRequired || (io.AScale.valid && io.BScale.valid))

  // MatrixTE drives this interface as one lockstep input group.  Preserve the
  // established ready behavior for every member of that group, while the
  // input-valid predicate below remains the sole condition for a pipe0 write.
  io.AVector.ready := InReady
  io.BVector.ready := InReady
  io.CAdd.ready := InReady
  io.AScale.ready := InReady
  io.BScale.ready := InReady

  pipe0.io.aVector := io.AVector.bits
  pipe0.io.bVector := io.BVector.bits
  pipe0.io.cAdd := io.CAdd.bits
  pipe0.io.aScale := io.AScale.bits
  pipe0.io.bScale := io.BScale.bits
  pipe0.io.opcode := io.opcode
  pipe1.io.in := Pipe0ResReg
  pipe2.io.in := Pipe1ResReg
  pipe3.io.in := Pipe2ResReg
  pipe4.io.in := Pipe3ResReg
  pipe5.io.in := Pipe4ResReg
  pipe6.io.in := Pipe5ResReg
  pipe7.io.in := Pipe6ResReg
  pipe8.io.in := Pipe7ResReg

  when(Pipe8ResRegAllowIn) {
    PipeResRegValid(8) := PipeResRegValid(7)
    when(PipeResRegValid(7)) {
      Pipe8ResReg := pipe8.io.out
    }
  }

  when(Pipe7ResRegAllowIn) {
    PipeResRegValid(7) := PipeResRegValid(6)
    when(PipeResRegValid(6)) {
      Pipe7ResReg := pipe7.io.out
    }
  }

  when(Pipe6ResRegAllowIn) {
    PipeResRegValid(6) := PipeResRegValid(5)
    when(PipeResRegValid(5)) {
      Pipe6ResReg := pipe6.io.out
    }
  }

  when(Pipe5ResRegAllowIn) {
    PipeResRegValid(5) := PipeResRegValid(4)
    when(PipeResRegValid(4)) {
      Pipe5ResReg := pipe5.io.out
    }
  }

  when(Pipe4ResRegAllowIn) {
    PipeResRegValid(4) := PipeResRegValid(3)
    when(PipeResRegValid(3)) {
      Pipe4ResReg := pipe4.io.out
    }
  }

  when(Pipe3ResRegAllowIn) {
    PipeResRegValid(3) := PipeResRegValid(2)
    when(PipeResRegValid(2)) {
      Pipe3ResReg := pipe3.io.out
    }
  }

  when(Pipe2ResRegAllowIn) {
    PipeResRegValid(2) := PipeResRegValid(1)
    when(PipeResRegValid(1)) {
      Pipe2ResReg := pipe2.io.out
    }
  }

  when(Pipe1ResRegAllowIn) {
    PipeResRegValid(1) := PipeResRegValid(0)
    when(PipeResRegValid(0)) {
      Pipe1ResReg := pipe1.io.out
    }
  }

  when(InReady) {
    PipeResRegValid(0) := inputValid
    when(inputValid) {
      Pipe0ResReg := pipe0.io.out
    }
  }

  io.DResult.valid := PipeResRegValid(8)
  io.DResult.bits := Pipe8ResReg.result
}

/** MatrixTE entry point with the established optional Scale-port convention. */
class FReducePE(implicit p: Parameters) extends CuteFpeModule {
  private val scaleBusWidth = ReduceWidth / MinDataTypeWidth / MinGroupSize * ScaleElementWidth

  val io = IO(new Bundle {
    val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
    val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
    val CAdd = Flipped(DecoupledIO(UInt(ResultWidth.W)))
    val AScale = Option.when(cuteFpeConfig.enableScalingFactor)(
      Flipped(DecoupledIO(UInt(scaleBusWidth.W)))
    )
    val BScale = Option.when(cuteFpeConfig.enableScalingFactor)(
      Flipped(DecoupledIO(UInt(scaleBusWidth.W)))
    )
    val DResult = DecoupledIO(UInt(ResultWidth.W))
    val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))
  })

  val core = Module(new FReduceCore)
  core.io.AVector.bits := io.AVector.bits
  core.io.AVector.valid := io.AVector.valid
  io.AVector.ready := core.io.AVector.ready
  core.io.BVector.bits := io.BVector.bits
  core.io.BVector.valid := io.BVector.valid
  io.BVector.ready := core.io.BVector.ready
  core.io.CAdd.bits := io.CAdd.bits
  core.io.CAdd.valid := io.CAdd.valid
  io.CAdd.ready := core.io.CAdd.ready

  core.io.AScale.bits := io.AScale.map(_.bits).getOrElse(0.U(scaleBusWidth.W))
  core.io.AScale.valid := io.AScale.map(_.valid).getOrElse(false.B)
  core.io.BScale.bits := io.BScale.map(_.bits).getOrElse(0.U(scaleBusWidth.W))
  core.io.BScale.valid := io.BScale.map(_.valid).getOrElse(false.B)
  io.AScale.foreach(_.ready := core.io.AScale.ready)
  io.BScale.foreach(_.ready := core.io.BScale.ready)

  core.io.opcode := Cat(0.U(1.W), io.opcode)

  // TODO: Move this compatibility concern into CMatrixReg once it can safely
  // schedule a same-bank C read and result write in one cycle.  The deeper
  // FPE pipeline changes the addr[0] phase between those two streams by an
  // odd number of cycles.  This elastic result register restores the legacy
  // parity phase without changing the internal pipe0 through pipe8 datapath.
  val OutputResRegValid = RegInit(false.B)
  val OutputResReg = Reg(UInt(ResultWidth.W))
  val OutputResRegAllowIn = !OutputResRegValid || io.DResult.ready

  core.io.DResult.ready := OutputResRegAllowIn
  when(OutputResRegAllowIn) {
    OutputResRegValid := core.io.DResult.valid
    when(core.io.DResult.valid) {
      OutputResReg := core.io.DResult.bits
    }
  }

  io.DResult.valid := OutputResRegValid
  io.DResult.bits := OutputResReg
}

class top(implicit p: Parameters) extends FReducePE {}
