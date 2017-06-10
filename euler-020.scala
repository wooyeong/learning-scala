println("ans: " + (BigInt(1) /: (1 to 100))(_ * _).toString.map(_.asDigit).sum)
