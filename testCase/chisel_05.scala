package AbstractCs

class MyModule extends Module {
  // 数据接口定义
  class DataBundle extends Bundle {
    val dataIn = Input(UInt(8.W))   // 数据输入
    val dataOut = Output(UInt(8.W)) // 数据输出
  }

  // 控制信号接口定义
  class ControlBundle extends Bundle {
    val enable = Input(Bool())      // 启用信号
    val reset = Input(Bool())       // 复位信号
  }

  // 定义 IO
  val io = IO(new Bundle {
    val data = new DataBundle       // 数据接口
    val control = new ControlBundle // 控制接口
  })

  val io1 = IO(new Bundle {
    val data = UInt(8.W) // 缺少 Input 或 Output
  })

  val io2 = new Bundle { // 缺少 IO()
    val data = Output(UInt(8.W))
  }

  // 功能逻辑
  io.data.dataOut := Mux(io.control.enable, io.data.dataIn, 0.U)
}
