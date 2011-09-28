/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

/** The `FutureTaskRunner</code> trait is a base trait of task runners
 *  that provide some sort of future abstraction.
 * 
 *  @author Philipp Haller
 */
trait FutureTaskRunner extends TaskRunner {

  /** The type of the futures that the underlying task runner supports.
   */
  type Future[T]

  /** An implicit conversion from futures to zero-parameter functions.
   */
  implicit def futureAsFunction[S](x: Future[S]): () => S

  /** Submits a task to run which returns its result in a future.
   */
  def submit[S](task: Task[S]): Future[S]

  /** Possibly blocks the current thread, for example, waiting for
   * a lock or condition.
   */
  def managedBlock(blocker: ManagedBlocker): Unit
  
  /** Sets the minimum, or target number of threads in this `FutureTaskRunner`.
   */
  def minimumPoolSize_=(size: Int): Unit
  
  /** Gets the minimum, or target number of threads in this `FutureTaskRunner`.
   */
  def minimumPoolSize: Int
  
  /** Sets the maximum allowable number of threads in this `FutureTaskRunner`.
   */
  def maximumPoolSize_=(size: Int): Unit
  
  /** Gets the maximum allowable number of threads in this `FutureTaskRunner`.
   */
  def maximumPoolSize: Int

  
}
