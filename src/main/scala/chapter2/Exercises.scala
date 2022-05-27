package chapter2

object Exercises extends App {
  def parallel[A, B](a: => A, b: => B): (A, B) = {
    class Executor[A](f: => A) extends Thread {
      @volatile var result: Option[A] = None
      override def run(): Unit = result = Some(f)
    }
    val th = new Executor(a)
    val th2 = new Executor(b)

    // Concurrent start (not parallel)
    th.start()
    th2.start()

    // Synchronized state
    th.join()
    th2.join()

    val res = for {
      aRes <- th.result
      bRes <- th2.result
    } yield (aRes, bRes)

    res.get
  }

  def periodically(duration: Long)(b: => Unit): Unit = {
    object JobScheduler extends Thread {
      var terminated = false
      val task: () => Unit = () => b
      override def run() =
        this.synchronized {
          while (!terminated) {
            task()
            this.wait(duration)
            run()
          }
        }

      def shutdown() = this.synchronized {
        terminated = true
        this.notify()
      }
    }
    JobScheduler.start()
  }

  println(parallel(println("ads"), println("adsad")))

  class SyncVar[T] {
    def get(): T = ???
}
