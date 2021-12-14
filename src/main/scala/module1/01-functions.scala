package module1

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object functions {


  /**
   * Функции
   */

   def sum(x: Int, y: Int): Int = x + y 

   val sum2: (Int, Int) => Int = (x, y) => x + y

   val sum3 = sum _

   sum(3, 2)  // 5

   sum2(3, 2) // 5

   val arrF = Array(sum2, sum2)

  /**
   * Реализовать ф-цию  sum, которая будет суммировать 2 целых числа и выдавать результат
   */



  // Currying

  val sumCurried: Int => (Int => Int) = sum2.curried

  val f1: Int => Int = sumCurried(5)
  val i = f1(3)  // 8


  
  // Partial function

  val pf: PartialFunction[String, Int] = {
    case x if x.toIntOption.nonEmpty => x.toInt
  }

  pf.isDefinedAt("1")  // true
  pf.isDefinedAt("foo")  // false

  Array("1", "foo", "2", "bar").collect(pf) // Array(1, 2)




  // SAM Single Abstract Method

  trait Printer{
    def foo(s: String): Unit
  }

  val p: Printer = s => println(s)

  p.foo("hello world")


  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */

   def isEven (x: Int) = x % 2 == 0


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */

  def isOdd(x: Int) = ! isEven(x)

  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */

  def filterEven(arr: Array[Int]): Array[Int] = {
    @tailrec
    def filterRec(arr: Array[Int], i: Int, resArr: Array[Int]): Array[Int] = arr match {
      case Array() => Array()
      case arr if i == arr.length => resArr
      case arr => if (isEven(arr(i))) {
        filterRec(arr, i + 1, arr(i) +: resArr)
      } else {
        filterRec(arr, i + 1, resArr)
      }
    }

    filterRec(arr, 0, Array())
  }

  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */
  def filterOdd(arr: Array[Int]): Array[Int] = {
    @tailrec
    def filterRec(arr: Array[Int], i: Int, resArr: ArrayBuffer[Int]): Array[Int] = arr match {
      case Array() => Array()
      case arr if i == arr.length => resArr.toArray
      // TODO: What to place instead of `!` to use only one tailRec func
      case arr => if (!isEven(arr(i))) {
        filterRec(arr, i + 1, arr(i) +: resArr)
      } else {
        filterRec(arr, i + 1, resArr)
      }
    }

    filterRec(arr, 0, ArrayBuffer())
  }

  /**
   * return statement
   *
   */



}
