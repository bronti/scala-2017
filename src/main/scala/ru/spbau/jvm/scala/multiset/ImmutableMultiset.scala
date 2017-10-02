package ru.spbau.jvm.scala.multiset

trait ImmutableMultiSet[+A] {
  def filter(pred: A => Boolean): ImmutableMultiSet[A]

  def map[B](maper: A => B): ImmutableMultiSet[B]

  def flatMap[B](mapper: A => ImmutableMultiSet[B]): ImmutableMultiSet[B]

  def fold[B >: A](ini: B)(op: (B, A) => B): B

  def foreach(func : A => Unit) : Unit

  def apply[B >: A](elem: B): Int

  def &[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B]

  def |[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B]

  def *(times: Int): ImmutableMultiSet[A]

  def +[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B]

  def find[B >: A](elem: B): Option[B]

  def size: Int
}

object ImmutableMultiSet {
  def apply[A](values: A*): ImmutableMultiSet[A] = {
    values.foldRight[ImmutableMultiSet[A]](EmptyImmutableMultiSet) { (value, multiSet) =>
      NodeImmutableMultiSet[A](value, multiSet)
    }
  }

  def unapplySeq[A](set: ImmutableMultiSet[A]): Option[Seq[A]] = { set match {
      case EmptyImmutableMultiSet => None
      case NodeImmutableMultiSet(head, tail) => Some(Seq(head) ++ unapplySeq(tail).getOrElse(Seq.empty))
    }
  }
}

case object EmptyImmutableMultiSet extends ImmutableMultiSet[Nothing] {

  override def filter(pred: (Nothing) => Boolean): ImmutableMultiSet[Nothing] = EmptyImmutableMultiSet

  override def map[B](maper: (Nothing) => B): ImmutableMultiSet[B] = EmptyImmutableMultiSet

  override def flatMap[B](mapper: (Nothing) => ImmutableMultiSet[B]): ImmutableMultiSet[Nothing] = EmptyImmutableMultiSet

  override def fold[B >: Nothing](ini: B)(op: (B, Nothing) => B): B = ini

  override def foreach(func : Nothing => Unit) : Unit = ()

  override def apply[B >: Nothing](elem: B): Int = 0

  override def &[B >: Nothing](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = EmptyImmutableMultiSet

  override def |[B >: Nothing](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = EmptyImmutableMultiSet

  override def *(times: Int): ImmutableMultiSet[Nothing] = EmptyImmutableMultiSet

  override def +[B >: Nothing](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = other

  override def find[B >: Nothing](elem: B) : Option[Nothing] = None

  override def size = 0
}

case class NodeImmutableMultiSet[+A](
                                      private val head: A,
                                      private val tail: ImmutableMultiSet[A]
                                    ) extends ImmutableMultiSet[A] {
  override def filter(pred: A => Boolean): ImmutableMultiSet[A] = {
    if (pred(head)) NodeImmutableMultiSet(head, tail.filter(pred))
    else tail.filter(pred)
  }

  override def map[B](mapper: A => B): ImmutableMultiSet[B] = NodeImmutableMultiSet(mapper(head), tail.map(mapper))

  override def flatMap[B](mapper: A => ImmutableMultiSet[B]): ImmutableMultiSet[B] = tail.flatMap(mapper) + mapper(head)

  override def fold[B >: A](ini: B)(op: (B, A) => B): B = op(tail.fold(ini)(op), head)

  override def foreach(func : A => Unit): Unit = {
    func(head)
    tail.foreach(func)
  }

  override def apply[B >: A](elem: B): Int = {
    if (head == elem) 1 + tail.apply(elem)
    else tail.apply(elem)
  }

  override def &[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = {
    val count = Math.min(other(head), this(head))
    ImmutableMultiSet(head) * count + (this.filter { !_.equals(head) } & other.filter { !_.equals(head) })
  }

  override def |[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = {
    val count = other(head) + this(head)
    ImmutableMultiSet(head) * count + (this.filter { !_.equals(head) } & other.filter { !_.equals(head) })
  }

  override def *(times: Int): ImmutableMultiSet[A] = {
    if (times == 0) EmptyImmutableMultiSet
    else this * (times - 1) + this
  }

  override def +[B >: A](other: ImmutableMultiSet[B]): ImmutableMultiSet[B] = {
    NodeImmutableMultiSet(head, tail + other)
  }

  override def find[B >: A](elem: B): Option[B] = {
    if (head == elem) Some(head)
    else tail.find(elem)
  }

  override def size: Int = 1 + tail.size
}
