package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f),map(r)(f))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => 1 + x + y)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((x,y) => x max y)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}