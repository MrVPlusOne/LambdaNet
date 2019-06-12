package lambdanet.utils

import collection._

/** Modified from 'https://www.scala-lang.org/docu/files/collections-api/collections-impl_6.html' */
class PrefixMap[K, V]
    extends mutable.Map[List[K], V]
    with mutable.MapLike[List[K], V, PrefixMap[K, V]] {

  var suffixes: immutable.Map[K, PrefixMap[K, V]] = Map.empty
  var value: Option[V] = None

  def get(s: List[K]): Option[V] =
    if (s.isEmpty) value
    else suffixes get s.head flatMap (_.get(s.tail))

  def withPrefix(s: List[K]): PrefixMap[K, V] =
    if (s.isEmpty) this
    else {
      val leading = s.head
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading).withPrefix(s.tail)
    }

  override def update(s: List[K], elem: V) =
    withPrefix(s).value = Some(elem)

  override def remove(s: List[K]): Option[V] =
    if (s.isEmpty) {
      val prev = value; value = None; prev
    } else suffixes get s.head flatMap (_.remove(s.tail))

  def iterator: Iterator[(List[K], V)] =
    (for (v <- value.iterator) yield (List(), v)) ++
      (for {
        (chr, m) <- suffixes.iterator
        (s, v) <- m.iterator
      } yield (chr +: s, v))

  def +=(kv: (List[K], V)): this.type = { update(kv._1, kv._2); this }

  def -=(s: List[K]): this.type = { remove(s); this }

  override def empty = new PrefixMap[K, V]
}
