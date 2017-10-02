package ru.spbau.scala.MultiSet

trait MultiSet[+A] {
  def apply[B >: A](elem: B): MultiSet[B]

  def filter(predicate: A => Boolean): MultiSet[A]

  def map[B](function: A => B): MultiSet[B]

  def flatMap[B](function: A => MultiSet[B]): MultiSet[B]

  def find[B >: A](elem: B): Option[B]

  def getCount[B >: A](elem: B): Int

  def addDiffElementsToMultiSet[B >: A](other: MultiSet[B]): MultiSet[B]

  def unionOfDiffMultisets[B >: A](other: MultiSet[B]): MultiSet[B]

  def unionOfCommonElements[B >: A](other: MultiSet[B]): MultiSet[B]

  def |[B >: A](other: MultiSet[B]): MultiSet[B]

  def &[B >: A](other: MultiSet[B]): MultiSet[B]

  def foreach(func: A => Unit): Unit
}

case class NotEmptyMultiSet[+A](value: A, count: Int, next: MultiSet[A]) extends MultiSet[A] {

  override def apply[B >: A](elem: B): MultiSet[B] = find(elem) match {
    case None =>
      new NotEmptyMultiSet[B](elem, 1, this)
    case Some(_) =>
      if (elem == value)
        new NotEmptyMultiSet[B](elem, count + 1, next)
      else
        next.apply(elem)
  }

  override def find[B >: A](elem: B): Option[B] = {
    if (elem == value && count != 0) {
      Some(elem)
    } else {
      next.find(elem)
    }
  }

  override def filter(predicate: A => Boolean): MultiSet[A] = {
    if (predicate(value))
      new NotEmptyMultiSet[A](value, count, next.filter(predicate))
    else
      next.filter(predicate)
  }

  override def map[B](function: A => B): MultiSet[B] = {
    new NotEmptyMultiSet[B](function(value), count, next.map(function))
  }

  override def flatMap[B](function: A => MultiSet[B]): MultiSet[B] = {
    function(value) | next.flatMap(function)
  }

  def getCount[B >: A](elem: B): Int = {
    if (value == elem) {
      count
    } else {
      next.getCount(elem)
    }
  }

  override def unionOfDiffMultisets[B >: A](other: MultiSet[B]): MultiSet[B] = {
    if (next == EmptyMultiSet)
      new NotEmptyMultiSet[B](value, count, other)
    else
      new NotEmptyMultiSet[B](value, count, next.unionOfDiffMultisets(other))
  }

  override def |[B >: A](other: MultiSet[B]): MultiSet[B] = {
    val multisetWithElemsFromThis = addDiffElementsToMultiSet(other)
    val multisetWithElemsFromOther = other.addDiffElementsToMultiSet(this)
    multisetWithElemsFromOther
      .unionOfDiffMultisets(multisetWithElemsFromThis)
      .unionOfDiffMultisets(unionOfCommonElements(other))
  }

  override def addDiffElementsToMultiSet[B >: A](other: MultiSet[B]): MultiSet[B] = {
    if (count != 0 && other.getCount(value) == 0)
      new NotEmptyMultiSet[B](value, count, next.addDiffElementsToMultiSet(other))
    else
      next.addDiffElementsToMultiSet(other)
  }

  override def unionOfCommonElements[B >: A](other: MultiSet[B]): MultiSet[B] = {
    val otherCount = other.getCount(value)
    if (count != 0 && otherCount != 0)
      new NotEmptyMultiSet[B](value, count + otherCount, next.unionOfCommonElements(other))
    else
      next.unionOfCommonElements(other)
  }

  override def &[B >: A](other: MultiSet[B]): MultiSet[B] = {
    other.getCount(value) match {
      case 0 => next & other
      case otherCount =>
        if (count != 0)
          new NotEmptyMultiSet[B](value, Math.min(count, otherCount), next & other)
        else
          next & other
    }
  }

  override def foreach(func: (A) => Unit): Unit = {
    for (i <- 0 until count) {
      func(value)
    }
    next.foreach(func)
  }
}

case object EmptyMultiSet extends MultiSet[Nothing] {
  override def apply[B >: Nothing](elem: B): NotEmptyMultiSet[B] = new NotEmptyMultiSet[B](elem, 1, EmptyMultiSet)

  override def filter(predicate: (Nothing) => Boolean): MultiSet[Nothing] = EmptyMultiSet

  override def map[B](function: (Nothing) => B): MultiSet[Nothing] = EmptyMultiSet

  override def flatMap[B](function: (Nothing) => MultiSet[B]): MultiSet[Nothing] = EmptyMultiSet

  override def getCount[B >: Nothing](elem: B) = 0

  override def addDiffElementsToMultiSet[B >: Nothing](other: MultiSet[B]): MultiSet[Nothing] = EmptyMultiSet

  override def find[B >: Nothing](elem: B): Option[B] = None

  override def unionOfDiffMultisets[B >: Nothing](other: MultiSet[B]): MultiSet[B] = other

  override def unionOfCommonElements[B >: Nothing](other: MultiSet[B]): MultiSet[B] = EmptyMultiSet

  override def |[B >: Nothing](other: MultiSet[B]): MultiSet[B] = other

  override def &[B >: Nothing](other: MultiSet[B]): MultiSet[Nothing] = EmptyMultiSet

  override def foreach(func: (Nothing) => Unit): Unit = {}
}

object MultiSet {
  def apply[A](elems: A*): MultiSet[A] = {
    var newMultiSet: MultiSet[A] = EmptyMultiSet
    for (elem <- elems) {
      newMultiSet = newMultiSet.apply(elem)
    }
    newMultiSet
  }

  def unapplySeq[A](multiset: MultiSet[A]): Option[Seq[A]] = multiset match {
    case EmptyMultiSet => None
    case NotEmptyMultiSet(value, count, next) =>
      Some(Seq.fill(count)(value) ++ unapplySeq(next).getOrElse(Seq.empty))
  }
}