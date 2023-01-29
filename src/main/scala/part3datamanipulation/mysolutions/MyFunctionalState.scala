package part3datamanipulation.mysolutions

object MyFunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted: $currentCount"))
  val (eleven, counted) = countAndSay.run(10).value

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputaion = s"Multi a with 5, obtained: $a"
  // pure FP with states
  val incrementState = State((current: Int) => (current + 1, s"incremented $current, obtained ${current + 1}"))
  val multiState = State((current: Int) => (current * 5, s"multiplied $current with 5, obtained ${current * 5} "))

  val composedTransformation = firstComputation.flatMap{firstResult =>
    secondComputaion.map(secondResult => (firstResult, secondResult))
  }



  val forComprComposedResult = for {
    i <- incrementState
    m <- multiState
  } yield (i, m)

  val composedResult1 = forComprComposedResult.run(10)
  val composedResult2 = forComprComposedResult.run(10)



  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")

  val compositeResult3 = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  // TODO 1: an online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = {
    State((sc: ShoppingCart) => (ShoppingCart(item :: sc.items, sc.total + price), sc.total + price))
  }

  // TODO 2: pure mental gymnastics
  def inspect[A, B](f: A => B): State[A, B] = {
    State(value => (value, f(value)))
  }
  def get[A]: State[A, A] = {
    State(current => (current, current))
  }
  def set[A](value: A): State[A, Unit] = {
    State(_ => (value, ()))
  }
  def modify[A](f: A => A): State[A, Unit] = {
    State(current => (f(current) ,()))
  }

  val program = for {
    a <- get[Int]
    _ <- set[Int] (a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 42)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println("Imperative approach")

    println("eleven = " + eleven)
    println("counted = " + counted)

    println("Functional approach")
    println(composedResult1.value)
    println(composedResult2.value)
    println("Composing of the function with andThen")
    println(compositeResult3(10))
    println("add to cart method")

    val myCart = for {
      _ <- addToCart("Table", 70)
      _ <- addToCart("Mouse", 50)
      total <- addToCart("Monitor", 200)
    } yield total

    println(myCart.run(ShoppingCart(List("keyboard"), 100)).value)
    println("methods demo")
    println(program.run(10).value)

  }
}
