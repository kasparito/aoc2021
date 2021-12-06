import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day6 extends Base(6):

  private val initialLanternFishAges = inputLines.head.split(",").map(age => (age.toInt, 1L)).toList

  @tailrec
  private def nextDay(fishAges: List[(Int, Long)], daysLeft: Int): Long =
    if daysLeft == 0 then
      fishAges.map(_._2).sum
    else
      nextDay(fishAges.groupBy(_._1).view.flatMap {
        case (age, fish) =>
          val n = fish.map(_._2).sum
          if age > 0 then
            Iterable((age - 1, n))
          else
            Iterable((6, n), (8, n))
      }.toList, daysLeft - 1)

  override def part1: Long = // 361169
    nextDay(initialLanternFishAges, 80)

  override def part2: Long = // 1634946868992
    nextDay(initialLanternFishAges, 256)
