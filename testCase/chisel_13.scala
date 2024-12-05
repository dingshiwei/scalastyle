import chisel3._

class TooManyWhenExample extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  when(io.in) {
    when(io.in) {
      when(io.in) {
        when(io.in) {
          when(io.in) { // 这里嵌套层级为 5，超过阈值
            io.out := false.B
          }
        }
      }
    }
  }
}
