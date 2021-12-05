import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day5 extends Base(5):

  private def inclusiveRange(i1: Int, i2: Int): Range =
    Range.inclusive(i1, i2, if i1 > i2 then -1 else 1)

  case class Point(x: Int, y: Int)

  case class LineSegment(p1: Point, p2: Point):
    private val horizontalRange = inclusiveRange(p1.x, p2.x)
    private val verticalRange = inclusiveRange(p1.y, p2.y)

    def straight: Boolean =
      p1.x == p2.x || p1.y == p2.y

    def pointsCovered: Set[Point] =
      if straight then
        (for x <- horizontalRange; y <- verticalRange yield Point(x, y)).toSet
      else
        horizontalRange.zip(verticalRange).map((x, y) => Point(x, y)).toSet

  object LineSegment:
    private val Pattern = raw"(\d+),(\d+) -> (\d+),(\d+)".r
    def parse(s: String): LineSegment =
      s match
        case Pattern(x1, y1, x2, y2) =>
          LineSegment(
            Point(x1.toInt, y1.toInt),
            Point(x2.toInt, y2.toInt))

  private val lineSegments = inputLines.map(LineSegment.parse)

  override def part1: Int = // 5294
    lineSegments
      .filter(_.straight)
      .flatMap(_.pointsCovered)
      .groupBy(identity)
      .count { case (_, points) => points.size >= 2 }

  override def part2: Int = // 21698
    lineSegments
      .flatMap(_.pointsCovered)
      .groupBy(identity)
      .count { case (_, points) => points.size >= 2 }
