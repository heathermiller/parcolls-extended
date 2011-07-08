import sys.process._

//run from /test/files/bench/collections, like this: scala RunRopes foreach

object RunRopes {

def runBenchmark(method: String): Unit = {
      println("[benchmark] Running SEQUENTIAL "+method+", "+method+"Rope...")
      val pr1 = Process("scala -cp immutable/classes/ "+method+"Rope 5")
      val str1 = pr1.!!
      println("[benchmark] Running SEQUENTIAL "+method+", "+method+"Vector...")
      val pr2 = Process("scala -cp immutable/classes/ "+method+"Vector 5")
      val str2 = pr2.!!
      println("[benchmark] Running SEQUENTIAL "+method+", "+method+"Array...")
      val pr3 = Process("scala -cp immutable/classes/ "+method+"Array 5")
      val str3 = pr3.!!
      println("[benchmark] Running PARALLEL "+method+", "+method+"ParRope...")
      val pr4 = Process("scala -cp parallel/immutable/classes/ "+method+"ParRope 5")
      val str4 = pr4.!!
      println("[benchmark] Running PARALLEL "+method+", "+method+"ParVector...")
      val pr5 = Process("scala -cp parallel/immutable/classes/ "+method+"ParVector 5")
      val str5 = pr5.!!
      println("[benchmark] Running PARALLEL "+method+", "+method+"ParArray...")
      val pr6 = Process("scala -cp parallel/immutable/classes/ "+method+"ParArray 5")
      val str6 = pr6.!!

      println("")
      println("RESULTS:")
      println(str1)
      println(str2)
      println(str3)
      println(str4)
      println(str5)
      println(str6)
}

def main(args: Array[String]){

  println("[compile] Compiling rope.scala benchmark...")
  val comp1 = Process("scalac -d immutable/classes/ immutable/rope.scala")
  println("[compile] Compiling parrope.scala benchmark...")
  val comp2 = Process("scalac -d parallel/immutable/classes parallel/immutable/parrope.scala")
  println("[compile] DONE.")

  args(0) match {
    case "foreach" => 
      val method = "foreach"
      runBenchmark(method)

    case "foldLeft" =>
      val method = "foldLeft"
      runBenchmark(method)

    case "filter" =>
      val method = "filter"
      runBenchmark(method)

    case "map" => 
      val method = "map"
      runBenchmark(method)

    case "aggregate" =>
      val method = "aggregate"
      runBenchmark(method)
  }
}
}
