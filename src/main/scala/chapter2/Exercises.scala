package chapter2

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

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
          sys.error("sync var has no value stored")
      }

      def put(x: T): Unit = store match {
        case Some(_) => sys.error("no more space for storing")
        case None    => store = Some(x)
      }
    }

  }

  object Exercise4 {
    class SyncVar[T] {
      @volatile var store: Option[T] = None

      def get(): T = store match {
        case Some(value) =>
          store = None
          value
        case None =>
          sys.error("sync var has no value stored")
      }

      def put(x: T): Unit = store match {
        case Some(_) => sys.error("no more space for storing")
        case None    => store = Some(x)
      }
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

      def put(x: T): Unit = store match {
        case Some(_) => sys.error("no more space for storing")
        case None =>
          store = Some(x)
          futureGet.synchronized {
            futureGet.notify()
          }
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
        override def run(): Unit = {
          println("Waiting for messages")
          val promise = syncVar.getWait()
          println(promise)
          run()
        }
      }
      val putNow = new Thread {
        val messages: mutable.Queue[Int] = mutable.Queue.from(0 until 15)
        override def run(): Unit = {
          println("Producing message")
          messages foreach syncVar.putWait
        }
      }
      getInFuture.start()
      Thread.sleep(5000)
      putNow.start()
    }
  }

  object Exercise6 {
    class SyncQueue[T](size: Int) {
      @volatile var store: mutable.Queue[T] = mutable.Queue.empty[T]
      private val futureGet = new AnyRef
      private val futurePut = new AnyRef

      def get(): T =
        Try(store.dequeue()) match {
          case Success(value) =>
            futurePut.synchronized {
              futurePut.notify()
            }
            value
          case Failure(_) =>
            sys.error("Sync queue has no value left")
        }

      def put(x: T): Unit = {
        if (store.sizeIs == size) sys.error("no more space for storing")
        store.enqueue(x)
        futureGet.synchronized { futureGet.notify() }
      }
      def isEmpty: Boolean = store.isEmpty
      def nonEmpty: Boolean = store.nonEmpty
      def getWait(): T = futureGet.synchronized {
        while (store.isEmpty) futureGet.wait()
        get()
      }
      def putWait(x: T): Unit = futurePut.synchronized {
        while (store.sizeIs >= size) futurePut.wait()
        put(x)
      }

    }

    def program = {
      val queue = new SyncQueue[Int](5)
      val producer = new Thread {
        override def run(): Unit = {
          0 until 13 foreach { n =>
            Thread.sleep(500)
            queue.putWait(n) // try with put to see the error
          }
        }
      }
      val consumer = new Thread {
        override def run(): Unit = {
          0 until 6 foreach { _ =>
            val cur = queue.getWait()
            println(cur)
          }
        }
      }
      consumer.start
      producer.start
    }
  }

  object Exercise7 {

    import scala.collection._

    private val transfers = mutable.ArrayBuffer[String]()

    def logTransfer(name: String, n: Int): Unit = transfers.synchronized {
      transfers += s"transfer to account '$name' = $n"
    }

    class Account(val name: String, @volatile var money: Int) {
      val uid = UniqueIdGiver.safeGetUniqueId()
    }

    def add(account: Account, n: Int): Unit = account.synchronized {
      account.money += n
      if (n > 10) logTransfer(account.name, n)
    }

    object UniqueIdGiver {
      private var id = 0

      def safeGetUniqueId(): Int = id.synchronized {
        val newId = id + 1
        id = newId
        id
      }
    }

    def send(a1: Account, a2: Account, n: Int) {
      def adjust() {
        a1.money -= n
        a2.money += n
      }

      if ( // because a1 & a2 can be received concurrently in the opposite order and deadlock
        a1.uid < a2.uid
      ) // You need to order them
        a1.synchronized {
          a2.synchronized {
            adjust()
          }
        } //
      else
        a2.synchronized {
          a1.synchronized {
            adjust()
          }
        }
    }

    def sendAll(accounts: Set[Account], target: Account): Unit = {
      accounts.toList
        .sortWith { case (c1, c2) => c1.uid < c2.uid }
        .foreach { acc =>
          println(
            s"-----------------BEFORE----------------------- \n" +
              s"SOURCE \nid: ${acc.uid}, name: ${acc.name}, money: ${acc.money} \n" +
              s"TARGET \nid: ${target.uid}, name: ${target.name}, money: ${target.money} \n"
          )
          if (target.uid < acc.uid) target.synchronized {
            acc.synchronized {
              transfer(target)(acc)
            }
          }
          else {
            acc.synchronized {
              target.synchronized {
                transfer(target)(acc)
              }
            }
          }
          println(
            s"-----------------AFTER------------------------ \n" +
              s"SOURCE \nid: ${acc.uid}, name: ${acc.name}, money: ${acc.money} \n" +
              s"TARGET \nid: ${target.uid}, name: ${target.name}, money: ${target.money} \n"
          )
        }

      def transfer(target: Account)(source: Account): Unit = {
        val amount = source.money
        target.money += amount
        source.money -= amount
      }

      /** Deberia sincronizar ordenado, siempre empezar con los mismos para
        * evitar mas de un sendAll
        *
        * Todas las operaciones deberian empezar con el mismo id
        */
    }

    def program = {
      val target = new Account("mario", 0)
      val source = Set(
        new Account("chueco", 10),
        new Account("brenda", 50),
        new Account("jorge", 90)
      )

      sendAll(source, target)
    }

    def program2 = {
      val target = new Account("mario", 0)
      val combinations = List
        .from[Int](10 until 20)
        .map(n => new Account("foo", n))
        .grouped(2)

      combinations.foreach { source =>
        new Thread {
          override def run(): Unit = sendAll(source.toSet, target)
        }.start()
      }
    }
  }

  object Exercise8 {
    object PriorityTaskPool {
      type Priority = Int
      type Task = () => Unit

      implicit val ord = Ordering.by[(Priority, Task), Int](_._1).reverse
      private val q = new mutable.PriorityQueue[(Priority, Task)]

      object Worker extends Thread {
        override def run(): Unit = q.synchronized {
          while (q.isEmpty) q.wait()
          val task = q.dequeue()._2
          task()
          run()
        }
      }

      def startWorkers = Worker.start()

      def asynchronous(priority: Int)(task: => Unit): Unit =
        q.synchronized {
          q.enqueue((priority, () => task))
          q.notify()
        }

    }

    def program = {
      PriorityTaskPool.asynchronous(2) {
        println("waiting low priority"); Thread.sleep(2000)
      }
      PriorityTaskPool.asynchronous(2) { println("saraza low priority") }
      PriorityTaskPool.asynchronous(1) { println("saraza high priority") }
      PriorityTaskPool.asynchronous(1) {
        println("waiting high priority"); Thread.sleep(2000)
      }
      PriorityTaskPool.asynchronous(1) {
        println("waiting high priority"); Thread.sleep(2000)
      }
      PriorityTaskPool.asynchronous(1) {
        println("waiting high priority"); Thread.sleep(2000)
      }
      PriorityTaskPool.startWorkers
    }
  }

  object Exercise9 {
    class PriorityTaskPool(workers: Int) {
      type Priority = Int
      type Task = () => Unit

      implicit val ord: Ordering[(Priority, Task)] =
        Ordering.by[(Priority, Task), Int](_._1).reverse
      private val q = new mutable.PriorityQueue[(Priority, Task)]
      private val lock = AnyRef

      private val workerPool = List.fill(workers)(new Worker)

      class Worker() extends Thread {
        override def run(): Unit = {
          lock.synchronized {
            while (q.isEmpty) lock.wait()
          }
          val task = q.dequeue()._2
          task()
          run()
        }
      }

      def startWorkers = workerPool.foreach { it =>
        it.start()
        log(s"Started ${it.getName}")
      }

      def asynchronous(priority: Int)(task: => Unit): Unit =
        lock.synchronized {
          q.enqueue((priority, () => task))
          lock.notify()
        }
    }
    def log(s: String) = {
      val th = Thread.currentThread()
      println(s"$th - $s")
    }
    def program = {
      val taskPool = new PriorityTaskPool(5)
      taskPool.asynchronous(1) { log("saraza high priority") }
      taskPool.asynchronous(1) {
        log("waiting high priority"); Thread.sleep(2000)
      }
      taskPool.asynchronous(2) {
        log("waiting low priority"); Thread.sleep(2000)
      }
      taskPool.asynchronous(2) { log("saraza low priority") }

      taskPool.asynchronous(1) {
        log("waiting high priority"); Thread.sleep(2000)
      }
      taskPool.asynchronous(1) {
        log("waiting high priority"); Thread.sleep(2000)
      }
      taskPool.startWorkers
    }
  }

  object Exercise10 {
    object PriorityTaskPool {
      type Priority = Int
      type Task = () => Unit
      implicit val ord: Ordering[(Priority, Task)] =
        Ordering.by[(Priority, Task), Int](_._1).reverse
    }
    class PriorityTaskPool(workers: Int)(important: Int) {
      import PriorityTaskPool._
      @volatile private var q = new mutable.PriorityQueue[(Priority, Task)]
      private val workerPool = List.fill(workers)(new Worker)
      private val enqueuelock = AnyRef
      private val dequeuelock = AnyRef

      class Worker() extends Thread {
        var terminated = false

        override def run(): Unit = {
          this.synchronized {
            while (q.isEmpty) this.wait()
            while (terminated && q.isEmpty) this.wait()
          }
          // Hacking around : I have no idea what im doing
          var task: () => Unit = () => ()
          dequeuelock.synchronized {
            task = q.dequeue()._2
          }
          task()
          run()
        }
      }

      def startWorkers = {
        workerPool.foreach { it =>
          if (it.getState == Thread.State.WAITING) {
            it.notify()
          } else {
            it.start()
            log(s"Started ${it.getName}")
          }
        }
      }

      def asynchronous(priority: Int)(task: () => Unit): Unit = {
        enqueuelock.synchronized {
          q.enqueue((priority, task))
        }
      }

      def shutdown(): Unit = dequeuelock.synchronized {
        q = q.filter(_._1 <= important)
        workerPool.foreach(it => it.terminated = true)
      }

      def join(): Unit = {
        workerPool.foreach(it => it.join())
      }
    }
    def log(s: String) = {
      val th = Thread.currentThread()
      println(s"$th - $s")
    }
    def program = {
      val taskPool = new PriorityTaskPool(5)(2)
      List
        .fill(10) { () => { log("waiting high priority"); Thread.sleep(2000) } }
        .foreach(task => taskPool.asynchronous(1)(task))
      List
        .fill(10) { () =>
          { log("saraza medium priority"); Thread.sleep(1500) }
        }
        .foreach(task => taskPool.asynchronous(2)(task))

      List
        .fill(10) { () => { log("saraza low priority"); Thread.sleep(1500) } }
        .foreach(task => taskPool.asynchronous(3)(task))
      List
        .fill(10) { () =>
          { log("saraza low-low priority"); Thread.sleep(1500) }
        }
        .foreach(task => taskPool.asynchronous(4)(task))

      taskPool.startWorkers
      taskPool.shutdown()
      taskPool.join()
    }
  }

  Exercise10.program
}
