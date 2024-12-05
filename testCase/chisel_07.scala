import chisel3._

val reg = Reg(UInt(8.W)) // 未初始化
val regInit = RegInit(0.U(8.W)) // 明确初始化
