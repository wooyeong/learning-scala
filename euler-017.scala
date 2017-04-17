object counter {
  val ones = List("one", "two", "three", "four",
      "five", "six", "seven", "eight", "nine", "ten")
  val elevs = List("eleven", "twelve", "thirteen", "fourteen",
      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  val tens = List("ten", "twenty", "thirty", "forty",
      "fifty", "sixty", "seventy", "eighty", "ninety")
  val hundred = "hundred"
  val and = "and"
  val thousand = "one thousand"

  val no = Array.ofDim[Int](ones.length)
  val nel = Array.ofDim[Int](elevs.length)
  val nt = Array.ofDim[Int](tens.length)
  var n100 = 0
  var nand = 0
  var n1000 = 0

  def apply(_n: Int) {
    var n = _n

    if (n == 1000) { n1000 += 1; n = 0 }

    if (n >= 100) {
      val h = n / 100 - 1
      no(h) += 1
      n100 += 1
      
      if (n % 100 != 0) nand += 1
      
      n = n % 100
    }

    if (n >= 20) {
      val t = n / 10 - 1
      nt(t) += 1

      n = n % 10
    }

    if (n >= 11) {
      n -= 11
      nel(n) += 1
      
      n = 0
    } 

    if (n != 0) { no(n - 1) += 1 }
  }

  def sum = {
    ones.map(_.length).zip(no).map { case (a,b) => a * b }.sum +
    elevs.map(_.length).zip(nel).map { case (a,b) => a * b }.sum +
    tens.map(_.length).zip(nt).map { case (a,b) => a * b }.sum +
    hundred.length * n100 +
    and.length * nand +
    thousand.replace(" ", "").length
  }
}

1 to 1000 foreach(n => counter(n))
println("ans: " + counter.sum)