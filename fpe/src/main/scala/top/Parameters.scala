package cute
// package cute

import chisel3._
import chisel3.util._
// import org.chipsalliance.cde.config._
// // import boom.exu.ygjk._
// import boom.v3.util._
import org.chipsalliance.cde.config._


trait DefaultCuteFpeParameters extends CuteFpeParameters {
  def cuteFpeConfig: CuteFpeConfig = CuteFpeConfig()

  def ReduceWidth :Int = 512
  def ResultWidth :Int = 32
  def MinGroupSize :Int = 16
  def MinDataTypeWidth : Int = 4
  def ScaleElementWidth : Int = 8
  def cmptreelayers :Int = 4
  def fp8cmptreelayers :Int = 4

  // 目前固定，与FP4刚好公用，不要动
  def P3AddNum :Int = ReduceWidth / 4 / MinGroupSize
  def P2AddNum :Int = ReduceWidth / (P3AddNum * 16)
  // val P4AddNum :Int = ReduceWidth / (P3AddNum * P2AddNum * 16)     // 刚好为1

  def FP4P0AddNum :Int = 2
  def FP4P1AddNum :Int = 16 / FP4P0AddNum

  def DEBUG_FP8 :Boolean = true
  def DEBUG_FP4 :Boolean = false
}

class Parameters{}

class CuteFpeModule(implicit val p: Parameters) extends Module with DefaultCuteFpeParameters
class CuteFpeBundle(implicit val p: Parameters) extends Bundle with DefaultCuteFpeParameters
