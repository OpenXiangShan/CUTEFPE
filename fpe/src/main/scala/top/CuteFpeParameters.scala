package cute

import chisel3._
import org.chipsalliance.cde.config._

object CuteFpeFormat {
  final val ResultWidth = 32
  final val MinGroupSize = 16
  final val MinDataTypeWidth = 4
  final val ScaleElementWidth = 8
}

trait CuteFpeParameters {
  final def ResultWidth: Int = CuteFpeFormat.ResultWidth
  final def MinGroupSize: Int = CuteFpeFormat.MinGroupSize
  final def MinDataTypeWidth: Int = CuteFpeFormat.MinDataTypeWidth
  final def ScaleElementWidth: Int = CuteFpeFormat.ScaleElementWidth
  final def P3AddNum: Int = ReduceWidth / 4 / MinGroupSize
  final def P2AddNum: Int = ReduceWidth / (P3AddNum * 16)
  final def FP4P1AddNum: Int = 16 / FP4P0AddNum

  def cuteFpeConfig: CuteFpeConfig

  def ReduceWidth: Int
  def fp8cmptreelayers: Int

  def FP4P0AddNum: Int

  def DEBUG_FP8: Boolean
  def DEBUG_FP4: Boolean
}

object DefaultCuteFpeParameters extends CuteFpeParameters {
  def cuteFpeConfig: CuteFpeConfig = CuteFpeConfig()

  def ReduceWidth: Int = 512
  def fp8cmptreelayers: Int = 4

  def FP4P0AddNum: Int = 2

  def DEBUG_FP8: Boolean = true
  def DEBUG_FP4: Boolean = false
}

case object CuteFpeParamsKey extends Field[CuteFpeParameters](DefaultCuteFpeParameters)

trait HasCuteFpeParameters extends CuteFpeParameters {
  implicit val p: Parameters

  private def fpeParams: CuteFpeParameters = p(CuteFpeParamsKey)

  def cuteFpeConfig: CuteFpeConfig = fpeParams.cuteFpeConfig

  def ReduceWidth: Int = fpeParams.ReduceWidth
  def fp8cmptreelayers: Int = fpeParams.fp8cmptreelayers

  def FP4P0AddNum: Int = fpeParams.FP4P0AddNum

  def DEBUG_FP8: Boolean = fpeParams.DEBUG_FP8
  def DEBUG_FP4: Boolean = fpeParams.DEBUG_FP4
}

class CuteFpeModule(implicit val p: Parameters) extends Module with HasCuteFpeParameters
class CuteFpeBundle(implicit val p: Parameters) extends Bundle with HasCuteFpeParameters
