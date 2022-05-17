package chapter2

object Exercises extends App {
  def parallel[A, B](a: => A, b: => B): (A, B) = {
    val a = thread(a)
    val b = thread(b)

  }

  def thread(f: => Unit): Thread = {
    val th = new Thread {
      override def run(): Unit = f
    }
    th.start()
    th
  }
}
