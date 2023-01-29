package part2abstractMath.mysolutions

import scala.annotation.tailrec

object MyCustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def pure[A](x: A): Option[A] = Option(x)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(value) => value match {
        case Left(l) => tailRecM(l)(f)
        case Right(r) => Some(r)
      }
      case None => None
    }
  }

  type Identity[T] = T
  val anumber: Identity[Int] = 300

  implicit object IdentityMonad extends Monad[Identity] {
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Right(r) => r
      case Left(l) => tailRecM(l)(f)
    }

    override def pure[A](x: A): Identity[A] = x
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A,B]]): Tree[B] = {
        t match {
          case Leaf(Left(v)) => stackRec(f(v))
          case Leaf(Right(v)) => Leaf(v)
          case Branch(left, right) => Branch(stackRec(left), stackRec(right))
        }
      }

      def tailRec(todo: List[Tree[Either[A,B]]], expanded: List[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(v)) => tailRec(todo.tail, Leaf(Right(v)) :: expanded, Leaf(v) :: done)
          case branch @ Branch(left, right) =>
            if (!expanded.contains(branch)) tailRec(left :: right :: todo, branch :: expanded, done)
            else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded.tail, newBranch :: done.drop(2))
            }
        }
        }


//      stackRec(f(a))
      tailRec(List(f(a)), Nil, Nil)
    }
//      f(a) match {
//      case Leaf(value) => value match {
//        case Left(v) => tailRecM(v)(f)
//        case Right(v) => Leaf(v)
//      }
//      case Branch(left, right) => ???
//    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }


  def main(args: Array[String]): Unit = {
    val anumber1: Identity[Int] = 300
    val result =   IdentityMonad.flatMap(anumber1)(x => x * 300)
    println(result)

  }

}
