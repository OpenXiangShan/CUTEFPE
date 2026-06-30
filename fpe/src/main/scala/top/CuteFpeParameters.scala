package cute

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

  def DEBUG_FP8: Boolean
  def DEBUG_FP4: Boolean
}

