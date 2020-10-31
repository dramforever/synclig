import synclig.types._
import synclig.builder.{Dsl}
import synclig.pprint.{pprint}

@main def play: Unit =
  import Dsl._
  import Expr._

  val res = run:
    val foo = reg("foo")
    val bar = reg("bar")

    bar := 0.U

    whilst(foo):
      bar := bar + 1.U
      skip

    loop:
      skip

  pprint(res)
