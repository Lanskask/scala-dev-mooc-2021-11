

val l = list.List[Int](1,2,3)

l.reverse()

l.cons(5)

l.mkString(", ")

val r = ", {1}$".r
val r2 = r.replaceAllIn("1, 2, 3, ", "")
r2

// fix 
// l.reverse()

l.map(x => x + 1)

// fix 
l
l.filter(x => x - 1 != 0)

list.incList(l)
list.shoutString(list.List("a", "b"))

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */

  //  case class List[T](head: T, tail: List[T])

  case object List {
    case class ::[+A](head: A, tail: List[A]) extends List[A]

    case object Nil extends List[Nothing]

    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    def apply[A](args: A*): List[A] = args match {
      case args if args.isEmpty => Nil
      case args if args.length == 1 => List.::(args.head, Nil)
      case args => List.::(args.head, apply[A](args.tail:_*))
    }

  }

  sealed trait List[+T] {
    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */
    def cons[A >: T](x: A): List[A] = List.::(x, this)
    
    def prepend[A >: T](x: A): List[A] = cons(x)

    
    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
//    @tailrec
    def mkString(delimeter: String): String = this match {
      case List.::(head, tail) => (head + delimeter + " " + tail.mkString(delimeter))
      case List.Nil => ""
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     * case class ::[+A](head: A, tail: List[A]) extends List[A]
     */
    def reverse(): List[T] = {
      def revRec[A](result: List[T], list: List[T]) : List[T] = {
        list match {
          case List.Nil => result
          case List.::(x, xs) => { revRec(List.::(x, result) , xs) }
        }
      }
      revRec(List.Nil, this)
    }

    /**/
    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: T => B): List[B] = this match {
      case List.Nil => List.Nil
      case List.::(head, tail) => List.::(f(head), tail.map(f))
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(condition: T => Boolean): List[T] = {
      def filterRec(init: List[T], filtered: List[T]): List[T] = init match {
        case List.Nil => filtered
        case List.::(head, tail) => if(condition(head)) {
          filterRec(tail, List.::(head, filtered))
        } else {
          filterRec(tail, filtered)
        }
      }

      filterRec(this, List.Nil) 
    }

  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(initList: List[Int]): List[Int] = {
    initList.map[Int](x => x + 1)
  }

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(initList: List[String]): List[String] = {
    initList.map[String](x => x + "!")
  }

}