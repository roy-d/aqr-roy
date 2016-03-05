package lab

import org.scalatest._

import scala.util.{Sorting, Random}

class AlgoSpec extends FlatSpec with MustMatchers {
  "Algo" should "power with 0 exponent" in {
    Algo.power(2,0) must ===(1)
  }

  it should "power with exponent as 1" in {
    Algo.power(2,1) must ===(2)
  }

  it should "power with even exponent" in {
    Algo.power(2,2) must ===(4)
  }

  it should "power with odd exponent" in {
    Algo.power(2,3) must ===(8)
  }

  it should "power with negative exponent" in {
    Algo.power(2,-1) must ===(1d/2)
    Algo.power(2,-2) must ===(1d/4)
    Algo.power(2,-3) must ===(1d/8)
  }

  it should "sort a smaller array" in {
    Algo.sort(Array(1,2,1,9)) must ===(Array(1,1,2,9))
    Algo.sort(Array(9,8,7,6,5,4,3,2,1)) must ===(Array(1,2,3,4,5,6,7,8,9))
  }

  it should "sort a large random array" in {
    val input = (1 to 1000000).toArray.map(_ => Random.nextInt(10)+1)
    val sortedInput = Algo.sort(input)
    Sorting.quickSort(input)
    sortedInput must === (input)
  }

  it should "split and sort input" in {
    Algo.sortSplits("AZQF013452BAB") must ===("AFQZ012345ABB")
    Algo.sortSplits("ZY98NM65BA21KJHG9432") must ===("YZ89MN56AB12GHJK2349")
  }

  it should "find largest palindrome" in {
    Algo.largestPalindrome("abba") must ===("abba")
    Algo.largestPalindrome("aaaabbaaaac") must ===("aaaabbaaaa")
    Algo.largestPalindrome("aaaabbaaa") must ===("aaabbaaa")
    Algo.largestPalindrome("caaaabbaaa") must ===("aaabbaaa")
    Algo.largestPalindrome("abcd") must === ("")
  }

}