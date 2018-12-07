package funcdiff

import scala.util.Random

object DataSet {
  def randomRepeat[T](data: Seq[T], random: Random): Iterator[T] = {
    Iterator.continually(random.shuffle(data)).flatten
  }
}
