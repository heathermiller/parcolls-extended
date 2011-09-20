/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel

import scala.util.control.Breaks._
import scala.concurrent.FutureTaskRunner
import scala.concurrent.ManagedBlocker

import annotation.unchecked.uncheckedVariance


  trait Task[R, +Tp] {
    type Result = R

    def repr = this.asInstanceOf[Tp]    
    
    /** Body of the task - non-divisible unit of work done by this task.
     *  Optionally is provided with the result from the previous completed task
     *  or `None` if there was no previous task (or the previous task is uncompleted or unknown).
     */
    def leaf(result: Option[R])    
    
    /** A result that can be accessed once the task is completed. */
    var result: R
    
    /** Decides whether or not this task should be split further. */
    def shouldSplitFurther: Boolean
    
    /** Splits this task into a list of smaller tasks. */
    private[parallel] def split: Seq[Task[R, Tp]]
    
    /** Read of results of `that` task and merge them into results of this one. */
    private[parallel] def merge(that: Tp @uncheckedVariance) {}
    
    // exception handling mechanism
    @volatile var throwable: Throwable = null
    def forwardThrowable() = if (throwable != null) throw throwable
    
    // tries to do the leaf computation, storing the possible exception
    private[parallel] def tryLeaf(lastres: Option[R]) {
      try {
        tryBreakable {
          leaf(lastres)
          result = result // ensure that effects of `leaf` are visible to readers of `result`
        } catchBreak {
          signalAbort
        }
      } catch {
        case thr: Exception =>
          result = result // ensure that effects of `leaf` are visible
          throwable = thr
          signalAbort
      }
    }
    
    private[parallel] def tryMerge(t: Tp @uncheckedVariance) {
      val that = t.asInstanceOf[Task[R, Tp]]
      val local = result // ensure that any effects of modifying `result` are detected
      // checkMerge(that)
      if (this.throwable == null && that.throwable == null) merge(t)
      mergeThrowables(that)
    }
    
    private def checkMerge(that: Task[R, Tp] @uncheckedVariance) {
      if (this.throwable == null && that.throwable == null && (this.result == null || that.result == null)) {
        println("This: " + this + ", thr=" + this.throwable + "; merged with " + that + ", thr=" + that.throwable)
      } else if (this.throwable != null || that.throwable != null) {
        println("merging this thr: " + this.throwable + " with " + that + ", thr=" + that.throwable)
      }
    }
    
    private[parallel] def mergeThrowables(that: Task[_, _]) {
      if (this.throwable != null && that.throwable != null) {
        // merge exceptions, since there were multiple exceptions
        this.throwable = this.throwable alongWith that.throwable
      } else if (that.throwable != null) this.throwable = that.throwable
      else this.throwable = this.throwable
    }
    
    // override in concrete task implementations to signal abort to other tasks
    private[parallel] def signalAbort() {}
  }
  
  trait TaskImpl[R, +Tp] {
    /** the body of this task - what it executes, how it gets split and how results are merged. */
    val body: Task[R, Tp]
    
    def split: Seq[TaskImpl[R, Tp]]
    /** Code that gets called after the task gets started - it may spawn other tasks instead of calling `leaf`. */
    def compute()
    /** Start task. */
    def start()
    /** Wait for task to finish. */
    def sync()
    /** Try to cancel the task.
     *  @return     `true` if cancellation is successful.
     */
    def tryCancel: Boolean
    /** If the task has been cancelled successfully, those syncing on it may
     *  automatically be notified, depending on the implementation. If they
     *  aren't, this release method should be called after processing the
     *  cancelled task.
     *
     *  This method may be overridden.
     */
    def release() {}
  }
  
/** A trait that declares task execution capabilities used
 *  by parallel collections.
 */
trait Tasks {
  
  private[parallel] val debugMessages = collection.mutable.ArrayBuffer[String]()
  
  private[parallel] def debuglog(s: String) = synchronized {
    debugMessages += s
  }
  
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]
  
  /* task control */
  
  // safe to assume it will always have the same type,
  // because the `tasksupport` in parallel iterable is final
  var environment: AnyRef
  
  /** Executes a task and returns a future. Forwards an exception if some task threw it. */
  def execute[R, Tp](fjtask: Task[R, Tp]): () => R
  
  /** Executes a result task, waits for it to finish, then returns its result. Forwards an exception if some task threw it. */
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R
  
  /** Retrieves the parallelism level of the task execution environment. */
  def parallelismLevel: Int
  
  /** Submits a blocking operation to the underlying pool to maintain its parallelism level. The behavior of this method depends on how the underlying thread pool maintains its parallelism level. */
  def managedBlock(mb: ManagedBlocker): Unit
  
}

