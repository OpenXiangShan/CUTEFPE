package cute

import chisel3._
import org.chipsalliance.cde.config._

trait CuteFpeParameters {
  def cuteFpeConfig: CuteFpeConfig

  def ReduceWidth: Int
  def ResultWidth: Int
  def MinGroupSize: Int
  def MinDataTypeWidth: Int
  def ScaleElementWidth: Int
  def cmptreelayers: Int
  def fp8cmptreelayers: Int

  def P3AddNum: Int
  def P2AddNum: Int
  def FP4P0AddNum: Int
  def FP4P1AddNum: Int
}

object DefaultCuteFpeParameters extends CuteFpeParameters {
  def cuteFpeConfig: CuteFpeConfig = CuteFpeConfig()

  def ReduceWidth: Int = 512
  def ResultWidth: Int = 32
  def MinGroupSize: Int = 16
  def MinDataTypeWidth: Int = 4
  def ScaleElementWidth: Int = 8
  def cmptreelayers: Int = 4
  def fp8cmptreelayers: Int = 4

  def P3AddNum: Int = ReduceWidth / 4 / MinGroupSize
  def P2AddNum: Int = ReduceWidth / (P3AddNum * 16)

  def FP4P0AddNum: Int = 2
  def FP4P1AddNum: Int = 16 / FP4P0AddNum
}

case object CuteFpeParamsKey extends Field[CuteFpeParameters](DefaultCuteFpeParameters)

trait HasCuteFpeParameters extends CuteFpeParameters {
  implicit val p: Parameters

  private def fpeParams: CuteFpeParameters = p(CuteFpeParamsKey)

  def cuteFpeConfig: CuteFpeConfig = fpeParams.cuteFpeConfig

  def ReduceWidth: Int = fpeParams.ReduceWidth
  def ResultWidth: Int = fpeParams.ResultWidth
  def MinGroupSize: Int = fpeParams.MinGroupSize
  def MinDataTypeWidth: Int = fpeParams.MinDataTypeWidth
  def ScaleElementWidth: Int = fpeParams.ScaleElementWidth
  def cmptreelayers: Int = fpeParams.cmptreelayers
  def fp8cmptreelayers: Int = fpeParams.fp8cmptreelayers

  def P3AddNum: Int = fpeParams.P3AddNum
  def P2AddNum: Int = fpeParams.P2AddNum
  def FP4P0AddNum: Int = fpeParams.FP4P0AddNum
  def FP4P1AddNum: Int = fpeParams.FP4P1AddNum
}

class CuteFpeModule(implicit val p: Parameters) extends Module with HasCuteFpeParameters
class CuteFpeBundle(implicit val p: Parameters) extends Bundle with HasCuteFpeParameters
