/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel

import scala.concurrent.FutureTaskRunner


trait TaskSupport extends Tasks

private[collection] class ForkJoinTaskSupport(runner: ForkJoinTaskRunner) extends AdaptiveWorkStealingForkJoinTasks(runner) with TaskSupport 

//private[collection] class ThreadPoolTaskSupport extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks

private[collection] class TaskRunnerTaskSupport(runner: FutureTaskRunner) extends AdaptiveWorkStealingTaskRunnerTasks(runner) with TaskSupport 
