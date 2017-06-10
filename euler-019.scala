// taken from http://www.scala-lang.org/api/current/scala/Enumeration.html
object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
import WeekDay._

// to use Ordering methods, such as `equiv`, `max`, or `min`
import scala.math.Ordering.Implicits._

class Date(val y: Int, val m: Int, val d: Int, private var _dow: Option[WeekDay] = None) extends Ordered[Date] {
    // require(1900 <= y && y <= 2010)
    require(1 <= m && m <= 12)
    require(1 <= d && d <= 31)

    def dow: Option[WeekDay] = this._dow
    def dow_=(dow: WeekDay) = _dow = Option(dow)
    def leapYear = Date.leapYear(y)

    def diff(that: Date): Int = that.compare(this) * Date.diffImpl(this min that, this max that)
    
    // Ordered
    override def compare(that: Date) = {
        if (this.y == that.y) {
            if (this.m == that.m) {
                this.d compare that.d
            } else this.m compare that.m
        } else this.y compare that.y
    }

    override def toString = s"${y}-${m}-${d} " + dow.getOrElse("N/A")
}

object Date {
    private val days = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    def apply(y: Int, m: Int, d: Int) = new Date(y, m, d, None)
    def fact(y: Int, m: Int, d: Int, dow: WeekDay) = new Date(y, m, d, Option(dow))
    
    private def diffImpl(from: Date, to: Date): Int = {
        //println(s"from $from to $to")
        if (from equiv to) 0
        else if (from.y == to.y && from.m == to.m) {
            //println(s"y.m, return ${to.d - from.d}")
            to.d - from.d
        } else if (from.y == to.y) {
            // month

            // (from ~ last day of the month)
            val ans = (diffImpl(from, Date(from.y, from.m, days(from.m-1) + (if (from.m == 2 && leapYear(from.y)) 1 else 0)))
            // + (next month of from ~ previous month of to)
            + (for (i <- from.m+1 until to.m) yield days(i-1) + (if (i == 2 && leapYear(from.y)) 1 else 0)).sum
            // + (first day of the month ~ to)
            + diffImpl(Date(to.y, to.m, 1), to) + 1)
            // println(s"y, return $ans")
            ans
        } else {
            // years
            
            // (from ~ 12/31 of from)
            val ans = (diffImpl(from, Date(from.y, 12, 31))
            // + for every years, (365) or (366)
            + (for (y <- from.y+1 until to.y) yield days.sum + (if (leapYear(y)) 1 else 0)).sum
            // + (1/1 ~ to)
            + diffImpl(Date(to.y, 1, 1), to) + 1)
            //println(s"N, return $ans")
            ans
        }
    }
    
    def leapYear(y: Int): Boolean = y % 4 == 0 && (y % 400 == 0 || y % 100 != 0)
}

var keep = Date.fact(1900, 1, 1, Mon)
var sundays = 0
// from 1901-01-01 to 2000-12-01
for (year <- 1901 to 2000; month <- 1 to 12) {
    val now = Date(year, month, 1)
    val dayPassed = keep.diff(now)

    now.dow = WeekDay(Math.floorMod(keep.dow.get.id + dayPassed, WeekDay.maxId))
    keep = now

    if (now.dow.get == Sun) sundays += 1
}
println("ans: " + sundays)