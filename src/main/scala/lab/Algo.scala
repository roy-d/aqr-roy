package lab

import scala.annotation.tailrec

object Algo {

  def power(p: Int, q: Int): Double = {
    @tailrec
    def evenPower(accum: Int, exp: Int): Int =
      if (exp == 1) accum
      else evenPower(accum * accum, exp / 2)

    if (q < 0) 1d / power(p, -1 * q)
    else if (q == 0) 1
    else if (q % 2 == 0) evenPower(p, q)
    else p * power(p, q - 1)
  }

  def sort(as: Array[Int]): Array[Int] = {
    val groups: Map[Int, Array[Int]] = as.groupBy(identity)
    (1 to 10)
      .filter(index => groups.contains(index))
      .map(index => groups(index))
      .flatten
      .toArray
  }

  def sortSplits(input: String): String = {
    val parts: Array[String] = input.split("((?<=[a-zA-Z])(?=[0-9]))|((?<=[0-9])(?=[a-zA-Z]))")
    val sortedParts: Array[String] = parts.map(part => part.toCharArray.sortWith(_ < _).mkString(""))
    sortedParts.mkString("")
  }

  def largestPalindrome(input: String): String = {

    @tailrec
    def isPalindrome(part: String): Boolean = {
      val length = part.length
      if (length == 0) true
      else if (length == 1) false
      else if (part.charAt(0) == part.charAt(length - 1)) isPalindrome(part.substring(1, length - 1))
      else false
    }

    val parts = for {
      start <- 0 to input.length
      end <- start to input.length
    } yield input.substring(start, end)

    val palindromes = parts.filter(isPalindrome)

    palindromes.sortBy(p => -1*p.length).head

  }


}
