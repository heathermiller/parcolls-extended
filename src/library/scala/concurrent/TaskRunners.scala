/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.{ThreadPoolExecutor, LinkedBlockingQueue, TimeUnit, Executors}

/** The `TaskRunners` object...
 *  
 *  @author Philipp Haller
 */
object TaskRunners {

  def makeCachedThreadPool(): FutureTaskRunner = {
    val exec = Executors.newCachedThreadPool
    JavaConversions.asTaskRunner(exec)
  }
  
  def makeForkJoinPool(): FutureTaskRunner = new ForkJoinTaskRunner
  def makeForkJoinPool(parallelism: Int): FutureTaskRunner = new ForkJoinTaskRunner(parallelism)
  def makeForkJoinPool(parallelism: Int, handler: UncaughtExceptionHandler, asyncMode: Boolean): FutureTaskRunner = new ForkJoinTaskRunner(parallelism, handler, asyncMode)
  
  def makeFixedThreadPool(nThreads: Int): FutureTaskRunner = {
    val exec = Executors.newFixedThreadPool(nThreads)
    JavaConversions.asTaskRunner(exec)
  }
  
  implicit val threadRunner: FutureTaskRunner =
    new ThreadRunner

  implicit val threadPoolRunner: FutureTaskRunner = {
    val numCores = Runtime.getRuntime().availableProcessors()
    val keepAliveTime = 60000L
    val workQueue = new LinkedBlockingQueue[Runnable]
    val exec = new ThreadPoolExecutor(numCores,
                                      numCores,
                                      keepAliveTime,
                                      TimeUnit.MILLISECONDS,
                                      workQueue,
                                      new ThreadPoolExecutor.CallerRunsPolicy)
    JavaConversions.asTaskRunner(exec)
  }

}
