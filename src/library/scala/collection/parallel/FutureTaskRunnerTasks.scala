/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel

import scala.concurrent.FutureTaskRunner


abstract class ResizingFutureTaskRunnerTasks(runner: FutureTaskRunner)  extends Tasks {

  abstract class ResizingFutureTaskRunnerTaskImpl[R, +Tp] extends Runnable with TaskImpl[R, Tp] {
    import runner._
  
    @volatile var owned = false
    @volatile var completed = false
    
    def start() = synchronized {
      // debuglog("Starting " + body)
      // utb: future = executor.submit(this)
      runner.synchronized {
        incrTasks
        runner.submit(() => this.run())
      }
    }
    def sync() = synchronized {
      // debuglog("Syncing on " + body)
      // utb: future.get()
      runner.synchronized {
        val coresize = runner.minimumPoolSize
        if (coresize < totaltasks) {
          runner.minimumPoolSize = coresize + 1         
          //assert(executor.getCorePoolSize == (coresize + 1))
        }
      }
      if (!completed) this.wait
    }
    def tryCancel = synchronized {
      // utb: future.cancel(false)
      if (!owned) {
        // debuglog("Cancelling " + body)
        owned = true
        true
      } else false
    }
    def run = {
      // utb: compute
      var isOkToRun = false
      synchronized {
        if (!owned) {
          owned = true
          isOkToRun = true
        }
      }
      if (isOkToRun) {
        // debuglog("Running body of " + body)
        compute
        release
      } else {
        // just skip
        // debuglog("skipping body of " + body)
      }
    }
    override def release = synchronized {
      completed = true
      runner.synchronized {
        decrTasks
      }
      this.notifyAll
    }
}  

  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]
  
  @volatile var totaltasks = 0
  
  private def incrTasks() = synchronized {
    totaltasks += 1
  }
  
  private def decrTasks() = synchronized {
    totaltasks -= 1
  }
  
  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing without wait: " + task)
    t.start
    
    () => {
      t.sync
      t.body.forwardThrowable
      t.body.result
    }
  }
  
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing with wait: " + task)
    t.start
    
    t.sync
    t.body.forwardThrowable
    t.body.result
  }
  
  var environment: AnyRef = runner
  
  def parallelismLevel = runner.minimumPoolSize  
}

/* An implementation of tasks objects based on the Java thread pooling API. */
/*
trait ThreadPoolTasks extends Tasks {
  import java.util.concurrent._
  
  trait TaskImpl[R, +Tp] extends Runnable with TaskImpl[R, Tp] {
    // initially, this is null
    // once the task is started, this future is set and used for `sync`
    // utb: var future: Future[_] = null
    @volatile var owned = false
    @volatile var completed = false
    
    def start() = synchronized {
      // debuglog("Starting " + body)
      // utb: future = executor.submit(this)
      executor.synchronized {
        incrTasks
        executor.submit(this)
      }
    }
    def sync() = synchronized {
      // debuglog("Syncing on " + body)
      // utb: future.get()
      executor.synchronized {
        val coresize = executor.getCorePoolSize
        if (coresize < totaltasks) {
          executor.setCorePoolSize(coresize + 1)
          //assert(executor.getCorePoolSize == (coresize + 1))
        }
      }
      if (!completed) this.wait
    }
    def tryCancel = synchronized {
      // utb: future.cancel(false)
      if (!owned) {
        // debuglog("Cancelling " + body)
        owned = true
        true
      } else false
    }
    def run = {
      // utb: compute
      var isOkToRun = false
      synchronized {
        if (!owned) {
          owned = true
          isOkToRun = true
        }
      }
      if (isOkToRun) {
        // debuglog("Running body of " + body)
        compute
        release
      } else {
        // just skip
        // debuglog("skipping body of " + body)
      }
    }
    override def release = synchronized {
      completed = true
      executor.synchronized {
        decrTasks
      }
      this.notifyAll
    }
  }
  
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp]
  
  var environment: AnyRef = ThreadPoolTasks.defaultThreadPool
  def executor = environment.asInstanceOf[ThreadPoolExecutor]
  def queue = executor.getQueue.asInstanceOf[LinkedBlockingQueue[Runnable]]
  @volatile var totaltasks = 0
  
  private def incrTasks() = synchronized {
    totaltasks += 1
  }
  
  private def decrTasks() = synchronized {
    totaltasks -= 1
  }
  
  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing without wait: " + task)
    t.start
    
    () => {
      t.sync
      t.body.forwardThrowable
      t.body.result
    }
  }
  
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing with wait: " + task)
    t.start
    
    t.sync
    t.body.forwardThrowable
    t.body.result
  }
  
  def parallelismLevel = ThreadPoolTasks.numCores
  
}

object ThreadPoolTasks {
  import java.util.concurrent._
  
  val numCores = Runtime.getRuntime.availableProcessors
  
  val tcount = new atomic.AtomicLong(0L)
  
  val defaultThreadPool = new ThreadPoolExecutor(
    numCores,
    Int.MaxValue,
    60L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable],
    new ThreadFactory {
      def newThread(r: Runnable) = {
        val t = new Thread(r)
        t.setName("pc-thread-" + tcount.incrementAndGet)
        t.setDaemon(true)
        t
      }
    },
    new ThreadPoolExecutor.CallerRunsPolicy
  )
}
*/

abstract class FutureTaskRunnerTaskImpl[R, +Tp](runner: FutureTaskRunner) extends Runnable with TaskImpl[R, Tp] {
  import runner._
    
  @volatile private var optFuture: Option[runner.Future[_]] = None
    
  def start() = {
    runner.synchronized {
      val future = runner.submit(() => this.run())
      optFuture = Some(future)
    }
  }
  def sync() = optFuture.get()
  def tryCancel = false
  def run = {
    compute
  }
}

abstract class FutureTaskRunnerTasks(runner: FutureTaskRunner) extends Tasks {
  
  protected def newTaskImpl[R, Tp](b: Task[R, Tp]): TaskImpl[R, Tp] // implemented as a subtrait/class of FutureTaskRunnerTasks  
  
  def execute[R, Tp](task: Task[R, Tp]): () => R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing without wait: " + task)
    t.start
    
    () => {
      t.sync
      t.body.forwardThrowable
      t.body.result
    }
  }
  
  def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = {
    val t = newTaskImpl(task)
    
    // debuglog("-----------> Executing with wait: " + task)
    t.start
    
    t.sync
    t.body.forwardThrowable
    t.body.result
  }
  
  var environment: AnyRef = runner
  
  def parallelismLevel =
    if (runner.minimumPoolSize == 0) Runtime.getRuntime().availableProcessors()
    else runner.minimumPoolSize
  
}




