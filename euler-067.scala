val input = scala.io.Source.fromFile("triangle.txt").getLines

val nums = input.toArray.reverse.map(_.split(' ').map(_.toInt))
val sum = nums.clone

for (i <- 1 until nums.length; j <- 0 until nums(i).length) {
  sum(i)(j) = (sum(i-1)(j) max sum(i-1)(j+1)) + nums(i)(j)
}

println("ans: " + sum.last.last) 