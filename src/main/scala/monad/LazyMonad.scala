package monad

object LazyMonad {
  def main(args: Array[String]): Unit = {
    val laz1 = new Lazy[Int]({
      println("It's new Lazy val")
      3 + 5
    })

    println(laz1.flatMap(x => new Lazy[Int](x)).get)
  }

  class Lazy[+A](v: => A) {
    private lazy val internal = v
    def flatMap[B](f: ( => A) => Lazy[B]): Lazy[B] = f(internal)

    def get: A = v
  }
}
