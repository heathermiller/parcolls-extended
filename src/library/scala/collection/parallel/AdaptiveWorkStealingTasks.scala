/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel

import scala.concurrent.FutureTaskRunner

  trait AdaptiveWorkStealingTaskImpl[R, Tp] extends TaskImpl[R, Tp] {
    @volatile var next: AdaptiveWorkStealingTaskImpl[R, Tp] = null
    @volatile var shouldWaitFor = true
    
    def split: Seq[AdaptiveWorkStealingTaskImpl[R, Tp]]
    
    def compute() = if (body.shouldSplitFurther) internal else body.tryLeaf(None)
    
    def internal() = {
      var last: AdaptiveWorkStealingTaskImpl[R, Tp] = spawnSubtasks()
      
      last.body.tryLeaf(None)
      body.result = last.body.result
      body.throwable = last.body.throwable
      
      while (last.next != null) {
        // val lastresult = Option(last.body.result)
        val beforelast = last
        last = last.next
        if (last.tryCancel) {
          // println("Done with " + beforelast.body + ", next direct is " + last.body)
          last.body.tryLeaf(Some(body.result))
          last.release
        } else {
          // println("Done with " + beforelast.body + ", next sync is " + last.body)
          last.sync
        }
        // println("Merging " + body + " with " + last.body)
        body.tryMerge(last.body.repr)
      }
    }
    
    def spawnSubtasks(): AdaptiveWorkStealingTaskImpl[R, Tp] = {
      var last: AdaptiveWorkStealingTaskImpl[R, Tp] = null
      var head: AdaptiveWorkStealingTaskImpl[R, Tp] = this
      do {
        val subtasks = head.split
        head = subtasks.head
        for (t <- subtasks.tail.reverse) {
          t.next = last
          last = t
          t.start
        }
      } while (head.body.shouldSplitFurther);
      head.next = last
      head
    }
    
    def printChain() = {
      var curr = this
      var chain = "chain: "
      while (curr != null) {
        chain += curr + " ---> "
        curr = curr.next
      }
      println(chain)
    }
  }
  
/** This trait implements scheduling by employing
 *  an adaptive work stealing technique.
 */
trait AdaptiveWorkStealingTasks extends Tasks {
  
  // specialize ctor
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): AdaptiveWorkStealingTaskImpl[R, Tp]
  
}

class AdaptiveWorkStealingForkJoinTaskImpl[R, Tp](val body: Task[R, Tp])
extends ForkJoinTaskImpl[R, Tp] with AdaptiveWorkStealingTaskImpl[R, Tp] {
  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new AdaptiveWorkStealingForkJoinTaskImpl[R, Tp](b)
  def split = body.split.map(b => newTaskImpl(b))
}

/* Some boilerplate due to no deep mixin composition. Not sure if it can be done differently without them.
 */
class AdaptiveWorkStealingForkJoinTasks(runner: ForkJoinTaskRunner) extends ForkJoinTasks(runner) with AdaptiveWorkStealingTasks {
  
  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new AdaptiveWorkStealingForkJoinTaskImpl[R, Tp](b)
  
}

class AdaptiveWorkStealingTaskRunnerTaskImpl[R, Tp](runner: FutureTaskRunner, val body: Task[R, Tp])
extends FutureTaskRunnerTaskImpl[R, Tp](runner) with AdaptiveWorkStealingTaskImpl[R, Tp] {
  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new AdaptiveWorkStealingTaskRunnerTaskImpl[R, Tp](runner, b)
  def split = body.split.map(b => newTaskImpl(b))
}

class AdaptiveWorkStealingTaskRunnerTasks(runner: FutureTaskRunner) extends FutureTaskRunnerTasks(runner) with AdaptiveWorkStealingTasks {

  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new AdaptiveWorkStealingTaskRunnerTaskImpl[R, Tp](runner, b)
  
}

class AdaptiveWorkStealingResizingTaskRunnerTasks(runner: FutureTaskRunner) extends ResizingFutureTaskRunnerTasks(runner) with AdaptiveWorkStealingTasks {

  class AdaptiveWorkStealingResizingTaskRunnerTaskImpl[R, Tp](val body: Task[R, Tp]) extends super.ResizingFutureTaskRunnerTaskImpl[R, Tp] with AdaptiveWorkStealingTaskImpl[R, Tp] {
    def split = body.split.map(b => newTaskImpl(b))
  }
  
  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new AdaptiveWorkStealingResizingTaskRunnerTaskImpl[R, Tp](b)
}

/*
trait AdaptiveWorkStealingThreadPoolTasks extends ThreadPoolTasks with AdaptiveWorkStealingTasks {
  
  class TaskImpl[R, Tp](val body: Task[R, Tp])
  extends super[ThreadPoolTasks].TaskImpl[R, Tp] with super[AdaptiveWorkStealingTasks].TaskImpl[R, Tp] {
    def split = body.split.map(b => newTaskImpl(b))
  }
  
  def newTaskImpl[R, Tp](b: Task[R, Tp]) = new TaskImpl[R, Tp](b)
  
}
*/



