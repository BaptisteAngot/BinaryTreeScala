sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def minimum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => minimum(l) min minimum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }

  def search[A](t: Tree[A], p: A => Boolean): Option[A] = t match {
    case Leaf(a) if p(a) => Some(a)
    case Leaf(_) => None
    case Branch(l, r) => search(l, p).orElse(search(r, p))
  }

  def isLeaf[A](t: Tree[A]): Boolean = t match {
    case Leaf(_) => true
    case _ => false
  }

  def foldRight[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l, foldRight(r, z)(f))(f)
  }

  def foldLeft[A, B](t: Tree[A], z: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
  }
}