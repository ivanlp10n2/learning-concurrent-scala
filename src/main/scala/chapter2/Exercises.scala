package chapter2

import scala.collection.mutable

object Exercises extends App {
  object Exercise1 {
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
    def program = println(parallel(println("ads"), println("adsad")))
  }

  object Exercise2 {
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
  }

  class Exercise3 {
    class SyncVar[T] {
      @volatile var store: Option[T] = None

      def get(): T = store match {
        case Some(value) =>
          store = None
          value
        case None =>
          store.get
      }

      def put(x: T): Unit = store = Some(x)
    }
  }

  class Exercise4 {
    class SyncVar[T] {
      @volatile var store: Option[T] = None

      def get(): T = store match {
        case Some(value) =>
          store = None
          value
        case None =>
          store.get
      }

      def put(x: T): Unit = store = Some(x)
      def isEmpty: Boolean = store.isEmpty
      def nonEmpty: Boolean = store.nonEmpty
    }
    val lock = new AnyRef

    class ProducerThread(syncVar: SyncVar[Int]) extends Thread {
      this.setDaemon(true)
      val messages: mutable.Queue[Int] = mutable.Queue.from(0 until 15)

      override def run(): Unit = lock.synchronized {
        if (messages.nonEmpty && syncVar.nonEmpty) lock.wait
        else if (messages.isEmpty) lock.wait
        else {
          println("Producing message")
          syncVar.put(messages.dequeue())
          lock.notify()
        }
        run()
      }
    }

    class ConsumerThread[A](syncVar: SyncVar[A]) extends Thread {
      this.setDaemon(true)

      override def run(): Unit = lock.synchronized {
        if (syncVar.isEmpty) lock.wait
        else {
          println("Consuming message")
          println(syncVar.get())
          lock.notify()
        }
        run()
      }
    }

    def program = {
      val sync = new SyncVar[Int]
      new ConsumerThread[Int](sync).start
      new ProducerThread(sync).start
    }
  }

  object Exercise5 {
    class SyncVar[T] {
      @volatile var store: Option[T] = None
      private val futureGet = new AnyRef
      private val futurePut = new AnyRef

      def get(): T = store match {
        case Some(value) =>
          futurePut.synchronized {
            store = None
            futurePut.notify()
            value
          }

        // Error branch
        case None =>
          sys.error("Sync var has no value stored")
      }

      def put(x: T): Unit = futureGet.synchronized {
        store = Some(x)
        futureGet.notify()
      }
      def isEmpty: Boolean = store.isEmpty
      def nonEmpty: Boolean = store.nonEmpty
      def getWait(): T = futureGet.synchronized {
        while (isEmpty) futureGet.wait()
        get()
      }
      def putWait(x: T): Unit = futurePut.synchronized {
        while (nonEmpty) futurePut.wait()
        put(x)
      }
    }

    def program = {
      val syncVar = new SyncVar[Int]
      val getInFuture = new Thread {
        override def run(): Unit = println(syncVar.getWait())
      }
      val putNow = new Thread {
        override def run(): Unit = syncVar.put(3)
      }
      getInFuture.start()
      Thread.sleep(5000)
      putNow.start()
    }
  }

}
