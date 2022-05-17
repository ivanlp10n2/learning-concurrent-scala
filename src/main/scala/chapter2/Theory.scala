package chapter2

object Theory {

  /** The memory model in Scala, its multithreading (Threads), and its
    * inter-thread synchronization (thread locks) are all inherited from the JVM
    */

  /** A memory model is a trade-off between the predictable behavior of a
    * concurrent program and a compiler's ability to perform optimizations. Not
    * every language or platform has a memory model. A typical purely functional
    * programming language, which doesn't support mutations, does not need a
    * memory model at all.
    */

  /** Starting a new JVM instance always creates only one process. Within the
    * JVM process, multiple threads can run simultaneously
    *
    * process has isolated memory spaces. two processes cannot read each others
    * memory directly or simultaneously use most of the resources
    */

  /** Calling the start method on a new thread notifies the OS that the thread
    * must start executing. Eventually results in executing the run method from
    * the new thread.
    *
    * When the OS decides to assign the new thread to some processor, this is
    * largely out of the programmer's control, but the OS must ensure that this
    * eventually happens.
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

  /** You can start a thread which will take a part of processor time in a given
    * time. If you use .join() it will wait until the thread has been executed
    * and finished. It sets your thread to WAITING STATE and it synchronizes its
    * finishing
    */

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
    def thread(thunk: => Unit): Thread = {
      val th = new Thread {
        thunk
      }
      th.start()
      th
    }

    def log(message: String): Unit = {
      System.out.println(message)
    }

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
        while (i < p.txt.length && !found)
          if (p.txt(i) == '!') {
            p.position = i
            found = true
          } else i += 1
      }
      while (!found) {}
      log(s"results: ${pages.map(_.position)}")
    }

    /** A volatile read is usually extremely cheap. In most cases, however, you
      * should resort to the synchronized statements; volatile semantics are
      * subtle and easy to get wrong. In particular, multiple volatile reads and
      * writes are not atomic without additional synchronization; volatiles
      * alone cannot help us to implement getUniqueId correctly
      */
  }

}
