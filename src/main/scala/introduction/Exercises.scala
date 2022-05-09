package introduction

import scala.util.Try

object Exercises extends App {
  def compose[A, B, C](g: B => C, f: A => B): A => C =
    a => g apply f(a)

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    for {
      n <- a
      m <- b
    } yield (n, m)

  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = {
    def recCheck: Boolean = xs match {
      case last +: Seq() => pred(last)
      case head +: tail  => pred(head) && check(tail)(pred)
    }

    Try(recCheck)
      .getOrElse(false)
  }

  println(
    check[Int](0 until 10)(40 / _ > 0)
  )

  case class Pair[P, Q](val first: P, val second: Q)

  def permutations(x: String): Seq[String] = {
    def perm(word: List[Char]): List[List[Char]] = {
      if (word.size == 1) List(word)
      else
        for {
          head <- word
          perms <- perm(word.filterNot(_ == head))
        } yield head :: perms
    }

    perm(x.toList).map(_.mkString)
  }

}
