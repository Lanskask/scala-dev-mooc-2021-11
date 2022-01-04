import scala.util.matching.Regex
//val isCyrillic = "['\\p{IsCyrillic}]*".r
//isCyrillic.matches("Hello")

val isCyr2 = "[U+0400–U+04FF]".r
isCyr2.matches("Hello")


