object NestedExample {
  def example(): Unit = {
    if (true) {       // 第一层
      if (true) {     // 第二层
        if (true) {   // 第三层
          println("Too deep")
        }
      }
    }
  }
}
