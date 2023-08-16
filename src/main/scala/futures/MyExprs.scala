package futures

object MyExprs extends App {

  def tuple[A, B](a: List[A], b: List[B]): List[(Any, Any)] =
    a.flatMap(xa => b.map(xb => (xa, xb)))

  def tuple2[A, B](a: List[A], b: List[B]): List[(Any, Any)] = for {
    xa <- a
    xb <- b
  } yield(xa, xb)

  val l1 = List(1,2,3)
  val l2 = List('A', 'B', 'C')

  println(tuple(l1,l2))
  println(tuple2(l1,l2))
  println(l1.zip(l2))
  println(l2.zipWithIndex)

  // def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
  //   a.flatMap(xa => b.map(xb => (xa, xb)))

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap(xa => b.map(xb => (xa, xb)))

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap(xa => b.map(xb => (xa, xb)))

  def tupleF[F[_], A, B](fa: F[A], fb: F[B]) = ???
    // fa.flatMap(xa => fb.map(xb => (xa, xb)))

  
  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1,2,3)
  val list2 = List(4,5,6)

  def optBindable[A](el: Option[A]): Bindable[Option, A] =
    new Bindable[Option, A] {
      override def flatMap[B](f: A => Option[B]): Option[B] = el.flatMap(f)
      override def map[B](f: A => B): Option[B] = el.map(f)
    }

  def listBindable[A](el: List[A]): Bindable[List, A] = 
    new Bindable[List, A] {
      def flatMap[B](f: A => List[B]): List[B] = el.flatMap(f)
      def map[B](f: A => B): List[B] = el.map(f)
    }


  println(tupleBindable[Bindable[Option[Int], Int], Int, Int](optA, optB))
  println(tupleBindable[Bindable[List[Int], Int], Int, Int](list1, list2))
}
