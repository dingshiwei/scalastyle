package examples

import chisel3._
import chisel3.util._

/** 模块示例：演示未正确同步跨时钟域信号的情况 */
class AsyncToSyncExample extends Module {
  val io = IO(new Bundle {
    val asyncSignal = Input(Bool())  // 来自异步时钟域的单比特信号
    val someOtherSignal = Input(Bool()) // 同步时钟域内的信号
    val processedSignal = Output(Bool()) // 输出处理后的信号
  })

  // 错误示例：未使用双同步器直接使用异步信号
  val async_to_sync = io.asyncSignal
  val processedSignal = async_to_sync && io.someOtherSignal
  io.processedSignal := processedSignal
}

/** 测试模块：用于验证未正确同步的错误逻辑 */
object AsyncToSyncExample extends App {
  println("Generating Verilog for AsyncToSyncExample...")
  (new chisel3.stage.ChiselStage).emitVerilog(new AsyncToSyncExample)
}
