package cute

object CuteFpeConfig {
  // FP16/TF32 input significands: 10 fraction bits plus the implicit leading bit.
  val MaxSignificandPrecision: Int = 11
  val FullProductWidth: Int = 2 * MaxSignificandPrecision + 1
  // FP32 accumulator significand: 23 fraction bits plus the implicit leading bit.
  val AccumulatorSignificandPrecision: Int = 24
  val ProductAlignmentPadding: Int =
    (AccumulatorSignificandPrecision - 1) - 2 * (MaxSignificandPrecision - 1)
}

case class CuteFpeConfig(
  enableTf32Fp32: Boolean = false,
  enableFp16Fp32: Boolean = false,
  enableMxfp4Fp32: Boolean = false,
  enableMxfp8Fp32: Boolean = false,
  enableNvfp4Fp32: Boolean = false,
  enableInt8Int32: Boolean = true
) {
  def enableScalingFactor: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32 || enableMxfp8Fp32

  def enableFp4withsf: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32
}
