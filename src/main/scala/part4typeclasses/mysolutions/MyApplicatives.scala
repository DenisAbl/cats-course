package part4typeclasses.mysolutions

object MyApplicatives {

  // Applicatives = Functors + the pure method

  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(987)
  val aMappedList = listApplicative.map(aList)(element => element * 2)

  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure("optionString")

  //import cats pure extension method
  import cats.syntax.applicative._
  val pureList: List[Int] = 2.pure[List] // List(2)
  val pureOption: Option[Int] = 2.pure[Option] //Some(2)

  // Monads extends Applicative
  // Applicative extends Functor
  // => Applicative extends Semigroupal

  import cats.data.Validated
  type ErrorOr[T] = Validated[List[String], T]
  val aValidThing: ErrorOr[Int] = 43.pure[ErrorOr]
  val transformedValidated: ErrorOr[Int] = aValidThing.map(_ - 40)

  val validatedApplicative: Applicative[ErrorOr] = Applicative[ErrorOr]
  // Applicatives have this ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicative extends Semigroupal
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])
                                        (implicit applicative: Applicative[W]): W[(A, B)] = {
    applicative.product(wa, wb)
  }

  val optionTuple = productWithApplicative(Option(2), Option("two"))
  val optionTuple2 = productWithApplicative(Option(2), None)


  def main(args: Array[String]): Unit = {
    println(aList)
    println(aMappedList)
    println(anOption)
    println(optionTuple)
    println(optionTuple2)
  }
}
