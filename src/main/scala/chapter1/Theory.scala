package chapter1

object Theory {

  // Armar el canal de charla
  // Armar meet

  // pensar en el ejmplo de escribir un examen 1,2,3
  // Countodwn latch
  // Todo lo que pasa 64bits puede ir en el stack
  /** In concurrent programming, we express a program as a set of concurrent
    * computations that execute during overlapping time intervals and coordinate
    * in some way
    */

  /**   - improve performance
    *   - much faster I/O operations. Improve responsiveness
    *   - Simplify the implementation and maintainability of computer programs
    *   - It can be more convenient to divide the program into smaller,
    *     independent computations than to incorporate everything into one large
    *     program
    */

  /**   - We assume concurrent programs communicate through the use of shared
    *     memory, and execute on a single computer.
    *   - A computer program that executes on multiple computers, each with its
    *     own memory, is called a distributed program. In distributed programs
    *     must assume that each of the computers can fail at any point, and
    *     provide some safety guarantees if this happens
    */

  /**   - Concurrency can manifest in the computer hardware, at the OS, or at
    *     the programming language.
    *   - Coordination of multiple executions in a concurrent system is called
    *     synchronization. Sync includes mechanisms used to order concurrent
    *     executions in time.
    *
    *   - In concurrent programs, different executions interact by modifying the
    *     shared memory subsystem of the computer.
    *   - In distributed programs, executions interact by message-passing
    *     communication
    */

  /** Processes and threads are the representations of concurrent executions.
    * They traditionally use locks and monitors to order parts of their
    * executions. Establishing an order between threads ensures that memory
    * modifications are visible to a threads that executes later.
    *
    * Nowadays exist better tools: communication channels, concurrent
    * collections, barriers, countdown latches, and thread pools.
    *
    * Old building blocks are prone to multiple kind of errors: deadlocks,
    * starvations, data races, and race conditions
    */

  /** computing a value concurrently requires creating a thread with a custom
    * run method, invoking the start method, waiting until the thread completes,
    * and then inspecting specific memory locations to read the result.
    *
    * what we really want to say is "compute some value concurrently, and inform
    * me when you are done.".
    *
    * Asynchronous programming using futures is a paradigm designed to
    * specifically these kind of statements
    *
    * reactive programming using event streams aims to declaratively express
    * concurrent computations that produce many values
    *
    * some high-level concurrency frameworks aim to transparently provide
    * distributed programming support as well. This is especially true for
    * data-parallel frameworks and message passing concurrency frameworks, such
    * as the actors
    */

  /** Such APIs emulate various programming models as embedded domain-specific
    * languages, with Scala serving as a host language: actors, software
    * transactional memory, and futures are examples of APIs that look like they
    * are basic language features, when they are in fact implemented as
    * libraries
    */

  /** The second reason Scala has pushed ahead lies in the fact that it is a
    * safe language. Automatic garbage collection, automatic bound checks, and
    * the lack of pointer arithmetic help to avoid problems such as memory
    * leaks, buffer overflows, and other memory errors.
    */

  /** Third reason is interoperability. JVM has well-defined threading and
    * memory models
    */

  /** Call stack: region of memory in which the program stores information about
    * the local variables and parameters of the currently executed methods
    *
    * object heap is the region of memory in which the objects are allocated by
    * the program
    */

  /** A process is an instance of a computer program that is being executed
    *
    * When the process starts, the OS reserves memory and other resources. Two
    * processes cannot read each other's memory directly or simultaneously use
    * most of the resources
    *
    * There are many more threads than processors. Every thread describes the
    * current state of the:
    *   - program stack
    *   - program counter
    *
    * during program execution
    *
    * OS threads are a programming facility provided by the OS. Unlike separate
    * processes, separate OS threads share a region of memory, and communicate
    * by writing and reading that memory
    */

  /** Every time a new JVM process starts, it creates several threads by
    * default. The most important among them is the main thread, which executes
    * the main method of the Scala program
    */

  val m: Thread = Thread.currentThread()
  val name = m.getName
  println(s"I am the thread $name")

  /** Threads goes through several thread states.
    *
    * When a Thread object is created is in new state.
    *
    * After the newly created starts exec, it goes into runnable state.
    *
    * After the thread is done executing it goes to terminated state
    */

  class MyThread extends Thread {
    override def run(): Unit = {
      println(" new thread created ")
    }
  }

  val t = new MyThread
  t.start()
  t.join()
  println("new thread joined.")

}
