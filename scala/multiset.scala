package akerblad

import collection._

// mutable multiset
class MultiSet[A] extends Traversable[(A,Int)] {
  private val map = new mutable.HashMap[A,Int]

  def add(a: A): Unit = {
    map.get(a) match {
      case Some(n) => map.put(a, n+1)
      case None => map.put(a, 1)
    }
  }

  def remove(a: A): Unit = {
    map.get(a) match {
      case Some(n) if n == 1 => map.put(a, n-1)
      case Some(n) if n > 1 => map.remove(a)
      case None => throw new NoSuchElementException(a.toString)
    }
  }

  // TODO: Implement these more efficiently
  def add(a: A, n: Int): Unit = for(i <- 0 until n) add(a)
  def remove(a: A, n: Int): Unit = for(i <- 0 until n) remove(a)

  override def foreach[U](f: ((A,Int)) => U) = map.foreach(f)
  def contains(a: A) = map.contains(a)

  def +=(a: A): Unit = add(a)
  def -=(a: A): Unit = remove(a)
  def ++=(as: TraversableOnce[A]): Unit = for(a <- as) this += a
  def ++=(as: MultiSet[A]): Unit = for( (a,count) <- as.map) add(a, count)
  def --=(as: TraversableOnce[A]): Unit = for(a <- as) this -= a
  def --=(as: MultiSet[A]): Unit = for( (a,count) <- as.map) remove(a, count)
  def apply(a: A): Int = map.get(a) match {
    case Some(n) => n
    case None => 0
  }
  //def toList(): List[(A,Int)] = map.keys.toList
  //def toFlatList?
  override def toString(): String = map.toString
}
