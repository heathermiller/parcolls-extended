/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import java.util.concurrent.Callable

import scala.concurrent.forkjoin._
import scala.concurrent._

/** The <code>ForkJoinTaskRunner</code> trait uses
 *  a <code>java.util.concurrent.ExecutorService</code>
 *  to run submitted tasks.
 *  
 *  @author Philipp Haller, Heather Miller
 */
private[scala] class ForkJoinTaskRunner(parallelism: Int,
							 	 handler: UncaughtExceptionHandler, 
							 	 asyncMode: Boolean
							 	 ) extends FutureTaskRunner {
  
  def this(parallelism: Int) = this(parallelism, null, false)
  def this() = this(Runtime.getRuntime().availableProcessors())
  
  type Task[T] = Callable[T] with Runnable
  type Future[T] = java.util.concurrent.Future[T]

  private class RunCallable[S](fun: () => S) extends Runnable with Callable[S] {
    def run() = fun()
    def call() = fun()
  }

  private[scala] val pool = {
    val p = new ForkJoinPool(parallelism, ForkJoinPool.defaultForkJoinWorkerThreadFactory)
    p.setUncaughtExceptionHandler(handler)
    p.setAsyncMode(asyncMode)
    p
  }
  
  implicit def functionAsTask[S](fun: () => S): Task[S] =
    new RunCallable(fun)

  implicit def futureAsFunction[S](x: Future[S]): () => S =
    () => x.get()

  def submit[S](task: Task[S]): Future[S] = {
    pool.submit[S](task)
  }

  def execute[S](task: Task[S]) {
    pool execute task
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  def shutdown() = pool.shutdown()


  def minimumPoolSize: Int = parallelism
      
  def minimumPoolSize_=(size: Int): Unit = { /*do nothing. The fork/join framework only allows sizing upon construction*/ }
      
  def maximumPoolSize: Int = Int.MaxValue
      
  def maximumPoolSize_=(size: Int): Unit = { /*do nothing. The fork/join framework has no maximum pool size property*/ }
 
}



