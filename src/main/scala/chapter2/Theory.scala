package chapter2

import chapter2.Theory.Threads.{getUniqueId, safeGetUniqueId}

import scala.collection.mutable

object Theory extends App {

  object Processes {

    /** The memory model in Scala, its multithreading (Threads), and its
      * inter-thread synchronization (thread locks) are all inherited from the
      * JVM
      */

    /** A memory model is a trade-off between the predictable behavior of a
      * concurrent program and a compiler's ability to perform optimizations.
      * Not every language or platform has a memory model. A typical purely
      * functional programming language, which doesn't support mutations, does
      * not need a memory model at all.
      */
  }

  object Threads {

    /** Starting a new JVM instance always creates only one process. Within the
      * JVM process, multiple threads can run simultaneously
      *
      * process has isolated memory spaces. two processes cannot read each
      * others memory directly or simultaneously use most of the resources
      */

    /** Calling the start method on a new thread notifies the OS that the thread
      * must start executing. Eventually results in executing the run method
      * from the new thread.
      *
      * When the OS decides to assign the new thread to some processor, this is
      * largely out of the programmer's control, but the OS must ensure that
      * this eventually happens.
      *
      * After the main thread starts the new thread t, it calls its join method.
      *
      * This method halts the execution of the main thread until t completes its
      * execution. We can say that the join operation puts the main thread into
      * the waiting state until t terminates. Importantly, the waiting thread
      * relinquishes its control over the processor, and the OS can assign that
      * processor to some other thread.
      */

    /** You don't know when the Thread is going to be executed. That's an OS
      * decision
      */

    /** You can start a thread which will take a part of processor time in a
      * given time. If you use .join() it will wait until the thread has been
      * executed and finished. It sets your thread to WAITING STATE and it
      * synchronizes its finishing
      */

    /** Thread states:
      *   - When a Thread object is created, it is initially in the new state
      *
      *   - After the newly created thread object starts executing, it goes into
      *     the runnable state
      *
      *   - After the thread is done executing, the thread object goes into the
      *     terminated state, and cannot execute any more
      */

    /** Thread.sleep puts the current thread into a TIMED WAITING STATE
      */

    /** A race condition is a phenomenon in which the output of a concurrent
      * program depends on the execution schedule of the statements in the
      * program. A race condition is not necessarily an incorrect program
      * behavior. However, if some execution schedule causes an undesired
      * program output, the race condition is considered to be a program error.
      */

    def thread(f: => Unit): Thread = {
      val th = new Thread {
        override def run(): Unit = f
      }
      th.start()
      th
    }

    def log(s: String): Unit = {
      val tname = Thread.currentThread()
      println(s"$tname - $s")
    }

    var idCounter = 0

    def getUniqueId(): Int = {
      val newId = idCounter + 1
      idCounter = newId
      newId
    }

    def undertermism = {
      def printUniqueIds(n: Int): Unit = {
        val uids = for (i <- 0 until n) yield getUniqueId()
        log(s"Generated uids: $uids")
      }

      val t = thread {
        printUniqueIds(5)
      }
      printUniqueIds(5)
    }

    undertermism

    /** ATOMICITY of race condition can be achieved with synchronized
      *
      * Every object created inside the JVM has a special entity called an
      * intrinsic lock or a monitor, which is used to ensure that only one
      * thread is executing some synchronized block on that object. When a
      * thread starts executing the synchronized block, we can say that the T
      * thread gains ownership of the x monitor, or alternatively, acquires it.
      * When a thread completes the synchronized block, we can say that it
      * releases the monitor
      *
      * Always synchronized > nothing. You can use higher abstraction too
      */
    def safeGetUniqueId() = this.synchronized(getUniqueId())
  }

  //  val b = new Threads

  object GuardedBlocks {

    import Threads._

    /** Creating threads is much more expensive than creating an object The same
      * thread should be reused for many requests; a set of such reusable
      * threads is usually called a thread pool
      */

    import scala.collection._

    object SynchronizedBadPool extends App {

      private val tasks = mutable.Queue[() => Unit]()

      val worker = new Thread {
        def poll(): Option[() => Unit] = tasks.synchronized {
          if (tasks.nonEmpty) Some(tasks.dequeue()) else None
        }

        override def run() = while (true) poll() match {
          case Some(task) => task()
          case None       =>
        }
      }
      worker.setName("Worker")
      worker.setDaemon(true)
      worker.start()

      def asynchronous(body: => Unit) = tasks.synchronized {
        tasks.enqueue(() => body)
      }

      asynchronous {
        log("Hello")
      }
      asynchronous {
        log(" world!")
      }
      Thread.sleep(5000)
    }

    /** This ends in busy-waiting executions. Instead of this, use wait and
      * notify which allow waiting and awakening the waiting threads,
      * respectively. It is only legal to call these methods on an x object if
      * the current thread owns the monitor of the object x. In other words,
      * wait and notify can only be called from a thread that owns the monitor
      * of that object. When a thread T calls wait on an object, it releases the
      * monitor and goes into the waiting state until some other thread S calls
      * notify on the same object
      */
    object SynchronizedGuardedBlocks extends App {
      val lock = new AnyRef
      var message: Option[String] = None
      val greeter = thread {
        lock.synchronized {
          while (message == None) lock.wait()
          log(message.get)
        }
      }
      lock.synchronized {
        message = Some("Hello!")
        lock.notify()
      }
      greeter.join()
    }

    /** An important property of the wait method is that it can cause spurious
      * wakeups. Occasionally, the JVM is allowed to wake up a thread that
      * called wait even though there is no corresponding notify call. To guard
      * against this, we must always use wait in conjunction with a while loop
      * that checks the condition, as in the previous example
      *
      * A synchronized statement in which some condition is repetitively checked
      * before calling wait is called a guarded block
      */
    object SynchronizedPool extends App {
      private val tasks = mutable.Queue[() => Unit]()

      object Worker extends Thread {
        setDaemon(true)

        def poll() = tasks.synchronized {
          while (tasks.isEmpty) tasks.wait()
          tasks.dequeue()
        }

        override def run() = while (true) {
          val task = poll()
          task()
        }
      }

      Worker.start()

      def asynchronous(body: => Unit) = tasks.synchronized {
        tasks.enqueue(() => body)
        tasks.notify()
      }

      asynchronous {
        log("Hello ")
      }
      asynchronous {
        log("World!")
      }
      Thread.sleep(500)
    }
  }

  object GreacefulShutdown {

    /** In the previous example, the Worker thread loops forever in its run
      * method and never terminates. You might be satisfied with this; Worker
      * does not use the CPU if it has no work to do, and since Worker is a
      * daemon thread, it is destroyed when the application exits. However, its
      * stack space is not reclaimed until the application terminates. If we
      * have a lot of dormant workers lying around, we might run out of memory.
      *
      * In the graceful shutdown, one thread sets the condition for the
      * termination and then calls notify to wake up a worker thread. The worker
      * thread then releases all its resources and terminates willingly.
      */
    object Worker extends Thread {
      var terminated = false
      private val tasks = mutable.Queue[() => Unit]()

      def poll(): Option[() => Unit] = tasks.synchronized {
        while (tasks.isEmpty && !terminated) tasks.wait()
        if (!terminated) Some(tasks.dequeue()) else None
      }

      import scala.annotation.tailrec

      @tailrec override def run() = poll() match {
        case Some(task) => task(); run()
        case None       =>
      }

      def shutdown() = tasks.synchronized {
        terminated = true
        tasks.notify()
      }
    }

    /** To ensure that various utility threads terminate correctly without race
      * conditions, use the graceful shutdown idiom
      *
      * The situation where calling interrupt is preferred to a graceful
      * shutdown is when we cannot wake the thread using notify. One example is
      * when the thread does blocking I/O on an InterruptibleChannel object, in
      * which case the object the thread is calling the wait method on is hidden
      */
  }

  object AtomicExecution {
    // ALWAYS SET THE OBJECT WHO HANDLES THE OPERATION with SYNCHRONIZED

    /** when used synchronized no other thread can run the same code. If anyone
      * else tries, they will get blocked until the runners end its execution.
      * Then JVM will decide who continues
      */

    /** Every object created inside the JVM has a special entity called an
      * intrinsic lock or a monitor, which is used to ensure that only one
      * thread is executing some synchronized block on that object. When a
      * thread starts executing the synchronized block, we can say that the T
      * thread gains ownership of the x monitor, or alternatively, acquires it.
      * When a thread completes the synchronized block, we can say that it
      * releases the monitor
      */

    /** Use the synchronized statement on some object x when accessing (reading
      * or modifying) a state shared between multiple threads. This ensures that
      * at most, a single T thread is at any time executing a synchronized
      * statement on x. It also ensures that all the writes to the memory by the
      * T thread are visible to all the other threads that subsequently execute
      * synchronized on the same object x.
      */
  }

  object MonitorsAndSynchronization {

    /** When used SYNCHRONIZED will ensure visibility of writes performed by
      * different threads and limit concurrent access to a shared region of
      * memory
      */

    /** A synchronization mechanism that enforces access limits on a shared
      * resource is called a lock
      *
      * Locks are also used to ensure that no two threads execute the same code
      * simultaneously; that is, they implement mutual exclusion
      *
      * each object on the JVM has a special built-in monitor lock, also called
      * the intrinsic lock. When a thread calls the synchronized statement on an
      * x object, it gains ownership of the monitor lock of the x object, given
      * that no other thread owns the monitor. Otherwise, the thread is blocked
      * until the monitor is released. Upon gaining ownership of the monitor,
      * the thread can witness the memory writes of all the threads that
      * previously released that monitor
      */
    object SynchronizedNesting extends App {
      import scala.collection._
      private val transfers = mutable.ArrayBuffer[String]()
      def logTransfer(name: String, n: Int) = transfers.synchronized {
        transfers += s"transfer to account '$name' = $n"
      }
    }
    import SynchronizedNesting.logTransfer
    class Account(val name: String, var money: Int)
    def add(account: Account, n: Int) = account.synchronized {
      account.money += n
      if (n > 10) logTransfer(account.name, n)
    }

    /** The add method calls logTransfer from inside the synchronized statement,
      * and logTransfer first obtains the transfers monitor. Importantly, this
      * happens without releasing the account monitor. If the transfers monitor
      * is currently acquired by some other thread, the current thread goes into
      * the blocked state without releasing its monitors
      */
    /** A deadlock is a general situation in which two or more executions wait
      * for each other to complete an action before proceeding with their own
      * action. The reason for waiting is that each of the executions obtains an
      * exclusive access to a resource that the other execution needs to
      * proceed.
      *
      * In concurrent programming, when two threads obtain two separate monitors
      * at the same time and then attempt to acquire the other thread's monitor,
      * a deadlock occurs.
      */
    object SynchronizedDeadlock extends App {
      import Threads._
      def send(a: Account, b: Account, n: Int) = a.synchronized {
        b.synchronized {
          a.money -= n
          b.money += n
        }
      } // acquire a lock, try to acquire b lock without releasing a
      val a = new Account("Jack", 1000)
      val b = new Account("Jill", 2000)
      val t1 = thread { for (i <- 0 until 100) send(a, b, 1) }
      val t2 = thread { for (i <- 0 until 100) send(b, a, 1) }
      t1.join(); t2.join()
      log(s"a = ${a.money}, b = ${b.money}")

    }

    /** Establish a total order between resources when acquiring them; this
      * ensures that no set of threads cyclically wait on the resources they
      * previously acquired.
      */
    class AccountOrdered(name: String, n: Int) extends Account(name, n) {
      val uid = safeGetUniqueId()
    }
    def send(a1: AccountOrdered, a2: AccountOrdered, n: Int) {
      def adjust() {
        a1.money -= n
        a2.money += n
      }
      if (a1.uid < a2.uid)
        a1.synchronized { a2.synchronized { adjust() } }
      else a2.synchronized { a1.synchronized { adjust() } }
    }

    /** Deadlocks are inherent to any concurrent system in which the threads
      * wait indefinitely for a resource without releasing the resources they
      * previously acquired
      *
      * The developer that resolved Jack's and Jill's issue was able to act
      * quickly by doing a heap dump of the running JVM instance and analyzing
      * the thread stacks; deadlocks can at least be easily identified
      */
  }

  object VolatileVars {
    import Threads._

    /** The JVM offers a more lightweight form of synchronization than the
      * synchronized block, called volatile variables. Volatile variables can be
      * atomically read and modified, and are mostly used as status flags; for
      * example, to signal that a computation is completed or cancelled. They
      * have two advantages. First, writes to and reads from volatile variables
      * cannot be reordered in a single thread. Second, writing to a volatile
      * variable is immediately visible to all the other threads.
      */
    class Page(val txt: String, var position: Int)

    object Volatile extends App {
      val pages =
        for (i <- 1 to 5) yield new Page("Na" * (100 - 20 * i) + " Batman!", -1)
      @volatile var found = false
      for (p <- pages) yield thread {
        var i = 0
        while (i < p.txt.length && !found) // busy-waiting
          if (p.txt(i) == '!') {
            p.position = i
            found = true
          } else i += 1
      }
      while (!found) {}
      log(s"results: ${pages.map(_.position)}")
    }

    /** Volatile lifts the variable into an object
      *
      * A volatile read is usually extremely cheap. In most cases, however, you
      * should resort to the synchronized statements; volatile semantics are
      * subtle and easy to get wrong. In particular, multiple volatile reads and
      * writes are not atomic without additional synchronization; volatiles
      * alone cannot help us to implement getUniqueId correctly
      */

  }

  object JMM {

    /** In the JMM, the different actions are (volatile) variable reads and
      * writes, acquiring and releasing object monitors, starting threads, and
      * waiting for their termination. If an action A happens before an action
      * B, then the action B sees A's memory writes. The same set of
      * happens-before relationships is valid for the same program irrespective
      * of the machine it runs on; it is the JVM's task to ensure this. We
      * already summarized most of these rules but we will now present a
      * complete overview:
      *
      * •Program order: Each action in a thread happens-before every other
      * subsequent action in the program order of that thread
      *
      * •Monitor locking: Unlocking a monitor happens-before every subsequent
      * locking of that monitor
      *
      * •Volatile fields: A write to a volatile field happens-before every
      * subsequent read of that volatile field
      *
      * •Thread start: A call to start() on a thread happens-before any actions
      * in the started thread
      *
      * •Thread termination: Any action in a thread happens-before another
      * thread completes a join() call on that thread
      *
      * •Transitivity: If an action A happens-before action B, and action B
      * happens-before action C, then action A happens-before action C
      */

    /** Despite its somewhat misleading name, the happens-before relationship
      * exists to ensure that threads see each other's memory writes. It does
      * not exist to establish a temporal ordering between different statements
      * in the program. When we say that a write A happens before a read B, it
      * is guaranteed that the effects of the write A are visible to that
      * particular read B. Whether or not the write A occurs earlier than the
      * read B depends on the execution of the program.
      */
  }

  object ImmutableObjectsAndFinalFields {

    /** We have said that programs must establish happens-before relationships
      * to avoid data races, but there is an exception to this rule. If an
      * object contains only final fields and the reference to the enclosing
      * object does not become visible to another thread before the constructor
      * completes, then the object is considered immutable and can be shared
      * between the threads without any synchronization
      */
  }

  import Threads._

  def imposibleConditionHappens =
    for (i <- 0 until 100000) {
      var a = false
      var b = false
      var x = -1
      var y = -1
      val t1 = thread {
        a = true
        y = if (b) 0 else 1
      }
      val t2 = thread {
        b = true
        x = if (a) 0 else 1
      }
      t1.join()
      t2.join()
      assert(!(x == 1 && y == 1), s"x = $x, y = $y")
    }
}
