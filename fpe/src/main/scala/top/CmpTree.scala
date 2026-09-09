package cute

import chisel3._
import chisel3.util._

/** The first half of a bit-mask exponent comparison tree. */
class FpeComparePrefix(elementNum: Int, prefixWidth: Int, remainingWidth: Int) extends Bundle {
  val mask = UInt(elementNum.W)
  val remaining = Vec(remainingWidth, UInt(elementNum.W))
  val maximum = UInt(prefixWidth.W)
}

/**
  * Bit-mask comparison helpers used at the Pipe1/Pipe2 boundary.
  *
  * A prefix comparison carries the surviving lane mask and the unexamined bit
  * planes through the register boundary.  Pipe2 resumes from that exact
  * candidate set instead of rebuilding a full comparator in a later stage.
  */
object FpeCmpTree {
  private def bitPlane(values: Seq[UInt], bit: Int): UInt =
    VecInit(values.map(_(bit))).asUInt

  private def select(mask: UInt, planes: Seq[UInt]): UInt = {
    planes.foldLeft(mask) { case (candidates, plane) =>
      val hit = candidates & plane
      Mux(hit.orR, hit, candidates)
    }
  }

  def prefix(values: Seq[UInt], width: Int, prefixWidth: Int): FpeComparePrefix = {
    require(values.nonEmpty, "FPE compare tree requires at least one input")
    require(prefixWidth > 0 && prefixWidth < width,
      s"invalid compare-tree prefix: $prefixWidth of $width bits")

    val elementNum = values.length
    val prefixPlanes = (0 until prefixWidth).map { offset =>
      bitPlane(values, width - 1 - offset)
    }
    val selected = select(Fill(elementNum, 1.B).asUInt, prefixPlanes)
    val out = Wire(new FpeComparePrefix(elementNum, prefixWidth, width - prefixWidth))

    out.mask := selected
    for (index <- 0 until width - prefixWidth) {
      out.remaining(index) := bitPlane(values, width - prefixWidth - 1 - index)
    }
    out.maximum := Cat(prefixPlanes.map(plane => (selected & plane).orR))
    out
  }

  def finish(mask: UInt, remaining: Seq[UInt]): UInt = {
    require(remaining.nonEmpty, "FPE compare tree finish requires remaining bits")
    val selected = select(mask, remaining)
    Cat(remaining.map(plane => (selected & plane).orR))
  }

  def mask(values: Seq[UInt], width: Int): UInt = {
    require(values.nonEmpty, "FPE compare tree requires at least one input")
    val planes = (0 until width).map { offset => bitPlane(values, width - 1 - offset) }
    select(Fill(values.length, 1.B).asUInt, planes)
  }

  def maximum(values: Seq[UInt], width: Int): UInt = {
    require(values.nonEmpty, "FPE compare tree requires at least one input")
    val planes = (0 until width).map { offset => bitPlane(values, width - 1 - offset) }
    val selected = select(Fill(values.length, 1.B).asUInt, planes)
    Cat(planes.map(plane => (selected & plane).orR))
  }
}
