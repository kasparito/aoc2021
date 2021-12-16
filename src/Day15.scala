import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.collection.mutable

object Day15 extends Base(15):

  private def findMinimalRisk(cavernRiskLevels: Map[(Int, Int), Long]): Long =
    val start = (0, 0)
    val stop = (cavernRiskLevels.keys.map(_._1).max, cavernRiskLevels.keys.map(_._1).max)

    case class Pos(pos: (Int, Int), risk: Long):
      def x: Int = pos._1
      def y: Int = pos._2

    val visited = mutable.Map.empty[(Int, Int), Long]

    val queue = mutable.PriorityQueue.empty(Ordering.by[Pos, Long](_.risk).reverse)
    queue.enqueue(Pos(start, 0L))
    while
      val Pos(pos@(x, y), accumulatedRisk) = queue.dequeue()
      if visited.get(pos).forall(_ > accumulatedRisk) then
        visited.put(pos, accumulatedRisk)
        for
          next <- Iterable((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))
          risk <- cavernRiskLevels.get(next)
        yield
          queue.enqueue(Pos(next, accumulatedRisk + risk))
      pos != stop
    do ()

    visited(stop)

  override def part1: Long = // 707
    findMinimalRisk({
      for
        (row, y) <- inputLines.zipWithIndex
        (risk, x) <- row.zipWithIndex
      yield
        (x, y) -> risk.toString.toLong
    }.toMap)

  override def part2: Long = // 2942
    findMinimalRisk({
      for
        ((row, dy), y) <- (0 until 5).flatMap(n => inputLines.map(_ -> n)).zipWithIndex
        ((risk, dx), x) <- (0 until 5).flatMap(n => row.map(_ -> n)).zipWithIndex
      yield
        (x, y) -> ((risk.toString.toLong + dx + dy - 1) % 9 + 1)
    }.toMap)
