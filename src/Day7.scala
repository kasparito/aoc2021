import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day7 extends Base(7):

  private val positions = inputLines.head.split(",").map(pos => pos.toInt).toList
  private val positionRange = positions.min to positions.max

  def cost(pos: Int): Int =
    positions.map(p => math.abs(pos - p)).sum

  def cost2(pos: Int): Int =
    positions.map { p => val x = math.abs(pos - p); x * (x + 1) / 2 }.sum

  override def part1: Int = // 364898
    positionRange.map(cost).min

  override def part2: Int = //
    positionRange.map(cost2).min
