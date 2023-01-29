package part4typeclasses.mysolutions

import cats.{Functor, Semigroupal}

object MyWeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val product2: W[(A, B)] = product(tuple._1, tuple._2)
      map(product2)(x => f(x._1, x._2))
    }

//    def ap2[W[_], B, T, C](wf: W[(B,T) => C])(wb: W[B], wt: W[T]): W[C]
    def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  }

 trait MyApplicative[W[_]] extends MyApply[W] {
   def pure[A](value: A): W[A]

 }

  import cats.Apply
  import cats.instances.option._

  val optionApply = Apply[Option]
  val optionFunction = optionApply.ap(Option((x: Int) => s"$x passed"))(Option(2))


  val regularApi = Option(33).map(x => "type changed")
  println(regularApi)
  println(optionFunction)

  def main(args: Array[String]): Unit = {

  }
}
