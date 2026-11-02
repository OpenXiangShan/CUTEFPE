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

case class CuteFpeMultiplierParams(width: Int, useTwosComplement: Boolean, padding: Int) {
  // Padding restores the floating-point scale; integer products bypass it.
  def productPadding: Int = 2 * padding
  // Float products omit constant low zeros; INT8/UINT8 products also fit this width.
  def productWidth: Int = CuteFpeConfig.FullProductWidth - productPadding
}

case class CuteFpeConfig(
  enableTf32Fp32: Boolean = false,
  enableFp16Fp32: Boolean = false,
  enableMxfp4Fp32: Boolean = false,
  enableMxfp8Fp32: Boolean = false,
  enableNvfp4Fp32: Boolean = false,
  enableInt8Int32: Boolean = true
) {
  def product0MultiplierParams: CuteFpeMultiplierParams = {
    // BF16/FP8 are always supported. MXFP8 shares FP8 significands; FP4 has its own path.
    val precision = Seq(
      Some(8),
      Option.when(enableFp16Fp32)(CuteFpeConfig.MaxSignificandPrecision),
      Option.when(enableTf32Fp32)(CuteFpeConfig.MaxSignificandPrecision)
    ).flatten.max
    makeMultiplierParams(precision)
  }

  def product1MultiplierParams: CuteFpeMultiplierParams = makeMultiplierParams(4)

  private def makeMultiplierParams(precision: Int): CuteFpeMultiplierParams = {
    // UINT8 needs eight magnitude bits plus a sign bit in the shared signed multiplier.
    val magnitudeWidth = if (enableInt8Int32) precision.max(8) else precision
    CuteFpeMultiplierParams(magnitudeWidth + (if (enableInt8Int32) 1 else 0),
      useTwosComplement = enableInt8Int32, padding = CuteFpeConfig.MaxSignificandPrecision - magnitudeWidth)
  }

  def enableScalingFactor: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32 || enableMxfp8Fp32

  def enableFp4withsf: Boolean =
    enableNvfp4Fp32 || enableMxfp4Fp32
}
