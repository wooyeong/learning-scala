val input = scala.io.Source.fromFile("names.txt")
val names = input.toVector.filter(_ != '"').mkString.split(',').sorted
val score = names.zipWithIndex.map { case (n, i) => (n.map(_.toInt - 'A' + 1).sum * (i+1))}
println("ans: " + score.sum)