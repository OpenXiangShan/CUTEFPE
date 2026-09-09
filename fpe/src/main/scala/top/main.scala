package cute

import chisel3._
import chisel3.experimental._
import org.chipsalliance.cde.config.Parameters
import java.io.{File, PrintWriter}

/** Emits the default legacy FPE wrapper for the standalone Verilator flow. */
object Main extends App {
  val writer = new PrintWriter(new File("top.v"))
  try {
    writer.write(getVerilogString(new top()(Parameters.empty)))
  } finally {
    writer.close()
  }
}
