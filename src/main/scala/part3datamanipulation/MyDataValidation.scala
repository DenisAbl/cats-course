package part3datamanipulation

import cats.Semigroup
import cats.data.Validated

import scala.util.Try

object MyDataValidation {

import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(2304) // Valid "right" value
  val invalidValue: Validated[String, Int] = Validated.invalid("Invalid value") // Undesirable "left" value
  val validationTest: Validated[String, Int] = Validated.cond(true, 23, "Invalid data")

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def isPrime(x: Int): Boolean = {
    @scala.annotation.tailrec
    def tailRec(d: Int): Boolean = {
      if (d <= 1) true
      else x % d != 0 && tailRec(d - 1)
    }
    if (x == 0 || x == 1 || x == -1) false
    else tailRec(Math.abs(x / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val primeInfo = if (isPrime(n)) Nil else List("Not a prime")
    val nonNegativeInfo = if (n >= 0) Nil else List("Negative")
    val greaterThanThresholdInfo = if (n <= 100) Nil else List("Greater than 100")
    val evenInfo = if (n % 2 == 0) Nil else List("Non even")

    if (n % 2 == 0 && n >= 0 && n <= 100 && isPrime(n)) Right(n)
    else Left(evenInfo ++ nonNegativeInfo ++ greaterThanThresholdInfo ++ primeInfo)
  }

  def validateNumber(n: Int): Validated[List[String], Int] = {
    implicit val intSemigroup: Semigroup[Int] = Semigroup.instance((x, _) => x)
    Validated.cond(isPrime(n), n, List("Not a prime"))
      .combine(Validated.cond(n >= 0, n, List("Negative")))
      .combine(Validated.cond(n <= 100, n, List("Greater than 100")))
      .combine(Validated.cond(n % 2 == 0, n, List("Not an even")))
  }

  //chain
  aValidValue.andThen(n => invalidValue)
  //test a valid value
  aValidValue.ensure("Bad")(_ >= 0)
  //transform
  aValidValue.map(_ * 100)
  aValidValue.leftMap(_.isEmpty)
  aValidValue.bimap(_.isEmpty, _ * 2)
  //stdlib actions
  val fromOption: Validated[List[String], Int] = Validated.fromOption(None, List("Empty response"))
  val fromOption2: Validated[List[String], Int] = Validated.fromOption(Some(222), List("Empty response"))
  val fromEither: Validated[List[String], Int] = Validated.fromEither(Right(222))
  val fromEither2: Validated[List[String], Int] = Validated.fromEither(Left(List("Error occurred")))
  val fromTry: Validated[Throwable, Double] = Validated.fromTry(Try("xxx".toDouble))

  aValidValue.toOption
  aValidValue.toEither

}
  // TODO 2 - form validation
  object FormValidation {

    type FormValidation[T] = Validated[List[String], T]
    def formValidation(form: Map[String, String]): FormValidation[String] = {
      def value(key: String): FormValidation[String] = {
        val formValue = form.get(key)
        Validated.cond(formValue.nonEmpty, formValue.get, List(s"No value for $key"))
      }

      def isNonEmptyValue(key: String, value: String) = Validated.cond(value.nonEmpty, value, List(s"$key is empty"))

      val dataPresentValidation: FormValidation[String] = value("name").combine(value("email")).combine(value("password"))

      val nonBlankName = isNonEmptyValue("name", form.getOrElse("name", ""))

      val emailValidation = Validated.cond(form.getOrElse("email", "").contains("@"), form.getOrElse("email", ""), List("Invalid email"))

      val passwordValidation = Validated.cond(form.getOrElse("password", "").length >= 10, form("password"), List("Password is invalid or too short"))

      //better to check each value separately andThen for each value make additional checks with combine
//      dataPresentValidation.andThen(_ => nonBlankName.combine(emailValidation).combine(passwordValidation))
      value("name").andThen(_ => nonBlankName)
        .combine(value("email").andThen(_ => emailValidation))
        .combine(value("password").andThen(_ => passwordValidation))
      .map(_ => "Success")
    }

    /*
      fields are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */


  def main(args: Array[String]): Unit = {
    println(MyDataValidation.testNumber(99))
    println(MyDataValidation.testNumber(2))
    println(MyDataValidation.testNumber(3))
    println(MyDataValidation.testNumber(-1000))
    println(MyDataValidation.testNumber(1001))

    println(MyDataValidation.validateNumber(99))
    println(MyDataValidation.validateNumber(2))
    println(MyDataValidation.validateNumber(3))
    println(MyDataValidation.validateNumber(-1000))
    println(MyDataValidation.validateNumber(1001))

    val form = Map(
      "name" -> "",
      "email" -> "danielrockthejvm.com",
      "password" -> "Rockthejvm1!"
    )

    val form1 = Map(
      "email" -> "danielrockthejvm.com",
      "password" -> "Rockthejvm1!"
    )

    val form2 = Map(
      "name" -> "Igorrrr",
      "password" -> "Rockthejvm1!"
    )

    val form3 = Map[String,String]()

    val form4 = Map(
      "name" -> "Daviiiiid",
      "email" -> "daniel@rockthejvm.com",
      "password" -> "Rockthejvm1!"
    )

    val form5 = Map(
      "email" -> "danielrockthejvm.com",
      "password" -> "Rq"
    )

    println
    println("Empty name, invalid email")
    println(FormValidation.formValidation(form))

    println("No name field: ")
    println(FormValidation.formValidation(form1))

    println("No email field: ")
    println(FormValidation.formValidation(form2))

    println("Empty map: ")
    println(FormValidation.formValidation(form3))
    println("Correct form")
    println(FormValidation.formValidation(form4))
    println(FormValidation.formValidation(form5))
  }
}
