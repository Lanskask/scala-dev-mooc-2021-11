package monad

object LazyMonad extends App {
  class Lazy[+A](v: => A) {
    def withFilter(f: A => Boolean): Lazy[A] = flatMap {
      case x if f(x) => Lazy(x)
    }

    private lazy val internal = v
    def flatMap[B](f: ( => A) => Lazy[B]): Lazy[B] = f(internal)
    def map[B](f: A => B): Lazy[B] = flatMap(x => Lazy(f(x)))
    def get: A = v
  }

  object Lazy {
    def apply[A](v: => A): Lazy[A] = new Lazy(v)
  }

  val laz1 = new Lazy[Int]({
    print("It's new Lazy val: ")
    3 + 5
  })

  val lazStr: Lazy[String] = laz1.flatMap(x => new Lazy[String](s"String: $x"))

  println(lazStr)
  println(lazStr.get)

  val laz4: Lazy[Int] = for {
    l1 <- Lazy(1)
    l2 <- Lazy(2)
    l3 <- Lazy(3)
  } yield l1 + l2 + l3

  val laz4Desugared: Lazy[Int] = Lazy(1)
    .flatMap(l1 =>
      Lazy(2)
        .flatMap(l2 =>
          Lazy(3)
            .withFilter(l3 => l1 >= 1)
            .map(l3 => l1 + l2 + l3)
        )
    )

  println(laz4Desugared)

  println(laz1.get)
  println(laz1.get)

  // monad laws
  // Associativity
  val f1Int2Str: Int => Lazy[String] = x => Lazy(s"String: $x")
  val f2Str2Dbl: String => Lazy[Double] = str => Lazy(str.length.toDouble)

  val r1 = Lazy(10).flatMap(i => f1Int2Str(i)).flatMap(st => f2Str2Dbl(st))
  val r2 = Lazy(10).flatMap(x => f1Int2Str(x).flatMap(st => f2Str2Dbl(st)))
  println(s"r1.get == r2.get: ${r1.get} == ${r2.get}: ${r1.get == r2.get}")
  assert(r1.get == r2.get)
}
