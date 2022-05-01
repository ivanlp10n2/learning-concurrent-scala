package introduction

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Exercises extends App {
  def compose[A, B, C](g: B => C, f: A => B): A => C =
    g compose f

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    for {
      n <- a
      m <- b
    } yield (n, m)

  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean =
    xs.forall(pred)

  case class Pair[P, Q](val first: P, val second: Q)

  def permutations(x: String): Seq[String] =
    x.permutations.toSeq

  def combinations(n: Int, xs: Seq[Int]): Iterator[Seq[Int]] =
    xs.combinations(n)

  def matcher(regex: String): PartialFunction[String, List[String]] = ???

}
