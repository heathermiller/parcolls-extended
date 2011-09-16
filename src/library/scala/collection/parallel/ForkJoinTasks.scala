/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel

import scala.concurrent.forkjoin._

/**
 * A trait describing objects that provide a fork/join pool.
 */
trait HavingForkJoinPool {
  def forkJoinPool: ForkJoinPool
}

trait ForkJoinTaskImpl[R, +Tp] extends RecursiveAction with TaskImpl[R, Tp] {
  def start() = fork
  def sync() = join
  def tryCancel = tryUnfork
}
  
/** An implementation trait for parallel tasks based on the fork/join framework.
 *  
 *  @define fjdispatch
 *  If the current thread is a fork/join worker thread, the task's `fork` method will
 *  be invoked. Otherwise, the task will be executed on the fork/join pool.
 */
abstract class ForkJoinTasks(runner: ForkJoinTaskRunner) extends Tasks with HavingForkJoinPool {
  
  // specialize ctor
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): ForkJoinTaskImpl[R, Tp]
  
  /** The fork/join pool of this collection.
   */
  def forkJoinPool: ForkJoinPool = environment.asInstanceOf[ForkJoinPool]
  var environment: AnyRef = runner.pool
  
  /** Executes a task and does not wait for it to finish - instead returns a future.
   *  
   *  $fjdispatch
   */
  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val fjtask = newTaskImpl(task)
    
    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }
    
    () => {
      fjtask.sync
      fjtask.body.forwardThrowable
      fjtask.body.result
    }
  }
  
  /** Executes a task on a fork/join pool and waits for it to finish.
   *  Returns its result when it does.
   * 
   *  $fjdispatch
   *  
   *  @return    the result of the task
   */
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val fjtask = newTaskImpl(task)
    
    if (Thread.currentThread.isInstanceOf[ForkJoinWorkerThread]) {
      fjtask.fork
    } else {
      forkJoinPool.execute(fjtask)
    }
    
    fjtask.sync
    // if (fjtask.body.throwable != null) println("throwing: " + fjtask.body.throwable + " at " + fjtask.body)
    fjtask.body.forwardThrowable
    fjtask.body.result
  }
  
  def parallelismLevel = forkJoinPool.getParallelism
  
}

object ForkJoinTasks {
  val defaultForkJoinPool: ForkJoinPool = new ForkJoinPool() // scala.parallel.forkjoinpool
  // defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors)
  // defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors)
}
