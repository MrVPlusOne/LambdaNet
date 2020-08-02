package funcdiff

import scala.concurrent.{Future, Promise}
import collection.concurrent.TrieMap

class BatchingService(f: CompNode => CompNode) {
  private val registered = TrieMap[CompNode, Promise[CompNode]]()

  def register(x: CompNode): Future[CompNode] = {
    val p = registered.getOrElseUpdate(x, {
      Promise[CompNode]()
    })
    p.future
  }

  def compute(): Unit = {
    val (xs, ps) = registered.toVector.unzip
    val ys = f(stackRows(xs))
    ys.rows.zip(ps).foreach {
      case (y, p) =>
        p.success(y)
    }
  }
}

object BatchingService {
  def apply(f: CompNode => CompNode) = new BatchingService(f)
}
