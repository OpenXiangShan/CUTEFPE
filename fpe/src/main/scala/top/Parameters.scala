package top
// package cute

import chisel3._
import chisel3.util._
// import org.chipsalliance.cde.config._
// // import boom.exu.ygjk._
// import boom.v3.util._

// 

object TopPara{
  val ReduceWidth = 512
  val ResultWidth = 32
  val cmptreelayers = 4
  val P3AddNum = 4
  val P2AddNum = ReduceWidth / (P3AddNum * 16)
}