package part4typeclasses.mysolutions

import cats.Apply


object MyWeakerMonads {
  import cats.Applicative

  trait MyFlatMap[M[_]] extends Apply[M]{
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO

    override def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(wf)(f => f(a)))
    //         |  |        /    \    \/
    //         |  |    M[A=>B] A=>B  B
    //         |  |    \_____   ____/
    //       M[A] A =>      M[B]
    }
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  def main(args: Array[String]): Unit = {




  }
}
