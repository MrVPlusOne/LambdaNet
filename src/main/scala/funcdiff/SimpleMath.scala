package funcdiff

import scala.util.Random

object SimpleMath {

  object Extensions {

    implicit class CollectionExtension[T](xs: Iterable[T]) {
      def any(p: T => Boolean): Boolean = {
        for (x <- xs) {
          if (p(x)) return true
        }
        false
      }
    }

    implicit class MapExtension[K, V](map: Map[K, V]) {
      def mapValuesNow[T](f: V => T): Map[K, T] = {
        map.mapValues(f).view.force
      }

      def compose(that: Map[K, V])(
        implicit subRule: SubstituteRule[K, V]
      ): Map[K, V] = {
        that ++ this.mapValuesNow(v => subRule.substitute(v, that))
      }
    }

    trait SubstituteRule[K, V] {
      def substitute(v: V, sub: Map[K, V]): V
    }
  }

  import SimpleMath.Extensions._

  def relu(x: Double): Double = if (x < 0) 0.0 else x

  def wrapInRange(i: Int, range: Int): Int = {
    val m = i % range
    if (m < 0) m + range else m
  }

  def wrapInRange(x: Double, size: Double): Double = {
    val m = x % size
    if (m < 0) m + size else m
  }

  def wrapInRange(x: Double, low: Double, high: Double): Double = {
    wrapInRange(x - low, high - low) + low
  }

  def clapInRange(from: Double, to: Double)(x: Double): Double = {
    if (x < from) from
    else if (x > to) to
    else x
  }

  def mean(xs: Seq[Double]): Double = {
    require(xs.nonEmpty)
    xs.sum / xs.length
  }

  def meanOrElse(xs: Seq[Double], default: Double): Double = {
    if (xs.nonEmpty) {
      xs.sum / xs.length
    } else default
  }

  def meanOpt(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  @inline
  def square(x: Double): Double = x * x

  @inline
  def cubic(x: Double): Double = x * x * x

  def gaussianForthOrder(halfPoint: Double)(x: Double): Double = {
    math.pow(2, -math.pow(x / halfPoint, 4))
  }

  def randomSelect[A](xs: IndexedSeq[A])(implicit random: Random): A = {
    val i = random.nextInt(xs.length)
    xs(i)
  }

  def randomGuess(random: Random)(pTrue: Double): Boolean = {
    random.nextDouble() < pTrue
  }

  def safeAbs(x: Int): Int = math.max(0, if (x < 0) -x else x)

  def expChoppedGaussian(chopMargin: (Double, Double), base: Double, powerE: Double)(
    random: Random
  ): Double = {
    val x = random.nextGaussian()
    if (x < chopMargin._1 || x > chopMargin._2) {
      expChoppedGaussian(chopMargin, base, powerE)(random)
    } else {
      math.pow(base, x * powerE)
    }
  }

  def natToList(n: Int, base: Int): List[Int] = {
    require(n >= 0)
    require(base > 0)
    def rec(n: Int): List[Int] = {
      if (n == 0) List()
      else {
        val residual = n % base
        residual :: rec(n / base)
      }
    }
    val r = rec(n).reverse
    if (r.isEmpty) List(0) else r
  }

  def aggressiveSigmoid(aggressiveness: Double): Real => Real = (x: Double) => {
    val a = math.pow(50, 2 * (0.5 - aggressiveness))
    math.pow(x, a)
  }

  def aggressiveInterpolate(aggressiveness: Double, from: Double, to: Double)(
    x: Double
  ): Real = {
    linearInterpolate(from, to)(aggressiveSigmoid(aggressiveness)(x))
  }

  def linearInterpolate(from: Double, to: Double)(x: Double): Double = {
    (to - from) * x + from
  }

  def expInterpolate(from: Double, to: Double, base: Double)(x: Double): Double = {
    val ratio = to / from
    val l = math.log(ratio) / math.log(base)
    from * math.pow(base, linearInterpolate(0, l)(x))
  }

  def sigmoid(x: Double): Real = 1.0 / (1 + math.exp(-x))

  def parallelMap[A, B](seq: Seq[A], threadNum: Int)(f: A => B): Seq[B] = {
    require(threadNum > 0)
    import scala.collection.parallel
    import parallel._

    val taskSupport = new ForkJoinTaskSupport(
      new scala.concurrent.forkjoin.ForkJoinPool(threadNum)
    )

    if (threadNum > 1) {
      val p = seq.par
      p.tasksupport = taskSupport
      p.map(f).toIndexedSeq
    } else {
      seq.map(f).toIndexedSeq
    }
  }

//  /** preserve the order of result (but the running order is not preserved) **/
//  def parallelArrayMap[A,B](threadNum: Int): (Seq[A], A => B) => Array[B] = {
//    require(threadNum>0)
//    import scala.collection.parallel
//    import parallel._
//
//
//    if(threadNum>1) {
//      val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threadNum))
//      (seq: Seq[A], f: A => B) => {
//        val result = new Array[B](seq.length)
//        val p = seq.zipWithIndex.par
//        p.tasksupport = taskSupport
//        p.foreach{ case (a, i) =>
//          result(i) = f(a)
//        }
//        result
//      }
//    }else{
//      (seq: Seq[A], f: A => B) => {
//        seq.map(f).toArray
//      }
//    }
//  }

  /**
    * Perform a map on a [[Seq]] in parallel. The order of starting each task is preserved as the [[Seq]].  */
  def parallelMapOrdered[A, B](seq: Seq[A], threadNum: Int, timeoutSec: Int = 20474834)(
    f: A => B
  ): Seq[B] = {
    require(threadNum > 0)

    if (threadNum > 1) {
      import akka.actor._

      import scala.concurrent.duration._

      object StartTask
      case class EvalTask(content: A)
      case class EvalResult(content: B)

      class MasterClass(supervisor: ActorRef, system: ActorSystem) extends Actor {
        val slaves: IS[ActorRef] =
          IS.fill(threadNum)(system.actorOf(Props(new SlaveActor())))
        var started = 0
        var finished = 0
        val tasks: IS[A] = seq.toIndexedSeq
        var results: List[B] = List()

        def getNextTask(): EvalTask = {
          val t = tasks(started)
          started += 1
          EvalTask(t)
        }

        def receive: PartialFunction[Any, Unit] = {
          case StartTask =>
            (0 until math.min(threadNum, tasks.length))
              .foreach(i => slaves(i) ! getNextTask())
          case EvalResult(b) =>
            results = b :: results
            finished += 1
            if (started < tasks.length) {
              sender() ! getNextTask()
            }
            if (finished == tasks.length)
              supervisor ! results.reverse
        }
      }

      class SlaveActor extends Actor {
        def receive: PartialFunction[Any, Unit] = {
          case EvalTask(content) =>
            val r = f(content)
            sender() ! EvalResult(r)
        }
      }

      val system = ActorSystem("parallelMap-system")
      val inbox = Inbox.create(system)
      val master = system.actorOf(Props(new MasterClass(inbox.getRef(), system)))
      master ! StartTask
      val result = inbox.receive(timeoutSec.seconds).asInstanceOf[Seq[B]]
      system.terminate().value
      result
    } else {
      seq.map(f).toIndexedSeq
    }
  }

  def processMap(args: Array[String], tasks: IS[Int], processNum: Int, mainClass: Object)(
    f: Int => Unit
  ): Unit = {
    if (args.isEmpty) {
      parallelMapOrdered(tasks, processNum) { i =>
        println(s"[JOB STARTED] id = $i")

        import java.io.File
        import java.nio.file.Paths
        val javaPath =
          Paths.get(System.getProperty("java.home"), "bin", "java").toFile.getAbsolutePath
        val jarPath = new File(
          this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath
        ).getAbsolutePath

        val mainClassName = {
          val s = mainClass.getClass.getName
          assert(s.last == '$')
          s.init
        }
        println(s"mainClassName = $mainClassName")
        val cmd = s"$javaPath -cp $jarPath $mainClassName $i" //be careful not to create fork bombs!
        println(cmd)

        import sys.process._
        cmd.split("\\s+").toSeq.!

        println(s"[JOB FINISHED] id = $i")
      }
    } else {
      f(args.head.toInt)
    }
  }

  def parallelMap[A, B](threadNum: Int): Seq[A] => (A => B) => IS[B] = {
    import scala.collection.parallel
    import parallel._
    val taskSupport = new ForkJoinTaskSupport(
      new scala.concurrent.forkjoin.ForkJoinPool(threadNum)
    )
    def parExecute(seq: Seq[A])(f: A => B): IS[B] = {
      if (threadNum > 1) {
        val p = seq.par
        p.tasksupport = taskSupport
        p.map(f).toIndexedSeq
      } else {
        seq.map(f).toIndexedSeq
      }
    }
    parExecute
  }

  def parallelMapWithResource[A, B, R](
    threadNum: Int
  )(xs: Seq[A], resources: IS[R])(f: (A, R) => B): IS[B] = {
    require(threadNum == resources.length)
    import collection.mutable

    val threadProblemMap = mutable.HashMap[Long, R]()
    var registeredThreadNum = 0

    SimpleMath.parallelMap(threadNum)(xs) { x =>
      val tId = Thread.currentThread().getId
      val resource = threadProblemMap.synchronized {
        threadProblemMap.getOrElse(tId, {
          val r = resources(registeredThreadNum)
          threadProblemMap(tId) = r
          registeredThreadNum += 1
          r
        })
      }
      f(x, resource)
    }
  }

  def normalizeDistribution(xs: IS[Double]): IS[Double] = {
    require(xs.nonEmpty)
    val sum = xs.sum
    xs.map(_ / sum)
  }

  case class PCF(pdf: IS[Double]) {
    val pcf: IS[Double] = {
      val normalizeFactor = pdf.sum
      var acc = 0.0
      pdf.map { p =>
        acc += p
        acc / normalizeFactor
      }
    }

    def sample(random: Random): Int = {
      val x = random.nextDouble()

      val i = pcf.indexWhere { pAcc =>
        x <= pAcc
      }
      if (i == -1) pcf.length - 1 else i
    }
  }

  case class Distribution[T](values: IS[T], pcf: PCF) {
    require(values.length == pcf.pdf.length)
    def sample(implicit random: Random): T = {
      values(pcf.sample(random))
    }
  }

  object Distribution {
    def fromPdf[T](ps: (T, Double)*): Distribution[T] = {
      val (ts, p) = ps.toIndexedSeq.unzip
      Distribution(ts, PCF(p))
    }
  }

  def maxwellDistribution(xPoints: IS[Double], temperature: Double): IS[Double] = {
    require(temperature > 0)
    normalizeDistribution(xPoints.map { x =>
      square(x) * math.exp(-x * x / temperature)
    })
  }

  /** measure the mutual information between two random variable `X` `Y`
    * @param frequencies a mapping given the number of each X,Y pair sampled from your distribution
    * */
  def mutualInformation[X, Y](frequencies: Map[(X, Y), Int]): Double = {
    import collection.mutable

    val totalCount = frequencies.values.sum
    val (xDomain, yDomain) = {
      val (xs, ys) = frequencies.keys.unzip
      (xs.toIndexedSeq.distinct, ys.toIndexedSeq.distinct)
    }

    val pxy = frequencies.mapValuesNow(_.toDouble / totalCount)

    val (px, py) = {
      val countX = mutable.HashMap(xDomain.map(x => x -> 0): _*)
      val countY = mutable.HashMap(yDomain.map(x => x -> 0): _*)
      frequencies.foreach {
        case ((x, y), n) =>
          assert(n > 0, s"n($x, $y) = 0, require n > 0. Please filter out empty pairs")
          countX(x) += n
          countY(y) += n
      }
      (
        countX.toMap.mapValuesNow(_.toDouble / totalCount),
        countY.toMap.mapValuesNow(_.toDouble / totalCount)
      )
    }

    (for {
      x <- xDomain
      y <- yDomain
      pxy0 <- pxy.get(x -> y)
    } yield {
      pxy0 * math.log(pxy0 / (px(x) * py(y)))
    }).sum / math.log(2)
  }

  def maxSmooth(data: Seq[Double]): Seq[Double] = {
    data.scanLeft(Double.MinValue)(math.max).tail
  }

  def randomSelectFrom[A](values: IS[A], maxPoints: Int, random: Random): IS[A] = {
    if (values.length <= maxPoints) {
      values
    } else {
      val xs = {
        var dataPointsLeft = maxPoints
        val filtered = values.indices.filter { i =>
          val keep =
            SimpleMath.randomGuess(random)(dataPointsLeft.toDouble / (values.length - i))
          if (keep) {
            dataPointsLeft -= 1
          }
          keep
        }
        if (filtered.last != (values.length - 1)) filtered :+ (values.length - 1)
        else filtered
      }
      xs.map(values.apply)
    }
  }

  /** In statistics, the coefficient of determination, denoted R2 or r2 and pronounced "R squared", is the proportion of the variance in the dependent variable that is predictable from the independent variable(s) */
  def rSquared(ys: IS[Double], predictions: IS[Double], weights: IS[Double]): Double = {
    val n = ys.length
    val mean = (0 until n).map(i => ys(i) * weights(i)).sum / n
    val resSquared = (0 until n).map(i => square(ys(i) - predictions(i)) * weights(i)).sum
    val variance = (0 until n).map(i => square(ys(i) - mean) * weights(i)).sum
    1 - resSquared / variance
  }

  def maxId(xs: IS[Double]): Int = {
    xs.zipWithIndex.maxBy(_._1)._2
  }

  def nearlyEqual(x: Double, y: Double): Boolean = {
    math.abs(x - y) <= 1e-8
  }

  case class RunningAverage(var value: Double, window: Int, var t: Int = 0) {

    def mix(x: Double): Unit = {
      t += 1
      val m = if (t <= window) 1.0 / t else 1.0 / window
      value = value * (1 - m) + x * m
    }
  }

  def mapSetZipWith[K, A, B, C](ms1: Map[K, Set[A]], ms2: Map[K, Set[B]])(
    f: (Set[A], Set[B]) => Set[C]
  ): Map[K, Set[C]] = {
    val keys = ms1.keySet ++ ms2.keySet
    keys.map { k =>
      k -> f(ms1.getOrElse(k, Set()), ms2.getOrElse(k, Set()))
    }.toMap
  }

  //  /** use exponentially weighted moving variance to calculate rSquared
  //    * @param alpha: change rate */
  //  def eRSquared(ys: IS[Double], predictions: IS[Double], weights: IS[Double], alpha: Double): Double = {
  //    val n = ys.length
  //    val residuals = (0 until n).map(i => predictions(i) - ys(i))
  //    var mean = 0.0
  //    var vSum = 0.0
  //    for(i <- 0 until n){
  //      val delta = residuals(i) - mean
  //      mean += alpha * weights(i) * delta
  //      vSum += square(delta) * weights(i)
  //    }
  //
  //    val staticMean = (0 until n).map(i => ys(i) * weights(i)).sum/n
  //    val variance = (0 until n).map(i => square(ys(i) - staticMean) * weights(i)).sum
  //    1 - vSum / variance
  //  }

  def main(args: Array[String]): Unit = {
    val counts = Map(
      (1, 1) -> 1,
      (2, 2) -> 1,
      (1, 2) -> 1,
      (2, 1) -> 1
    )
    println {
      mutualInformation(counts)
    }
  }
}
