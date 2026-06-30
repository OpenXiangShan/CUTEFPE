package cute

case class CuteFpeConfig(
  enableMxfp4Fp32: Boolean = false,
  enableMxfp8Fp32: Boolean = false,
  enableNvfp4Fp32: Boolean = false
) {
  def enableScalingFactor: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32 || enableMxfp8Fp32

  def enableFp4withsf: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32
}
