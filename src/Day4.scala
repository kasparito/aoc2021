import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day4 extends Base(4):

  case class BingoBoard(numbers: Array[Array[Int]]) {

    def isBingo(drawn: Set[Int]): Boolean =
      numbers.exists(_.forall(drawn)) ||
        numbers.transpose.exists(_.forall(drawn))

    def sumExcept(drawn: Set[Int]): Int =
      numbers.flatten.filterNot(drawn).sum

    override def toString: String =
      s"BingoBoard(${numbers.map(_.mkString(",")).mkString("\n")})"
  }

  private val draws = inputLines.head.split(',').map(_.toInt).inits.toSeq.reverse.collect {
    case prefix if prefix.length >= 5 =>
      ListSet(prefix: _*)
  }

  private val boards = inputLines.tail.grouped(6).map(_.filterNot(_.isBlank)).collect {
    case rows if rows.nonEmpty =>
      BingoBoard(rows.map(_.trim.split(raw"\s+").map(_.toInt).toArray).toArray)
  }.toList

  @tailrec
  private def findLastBoardScore(boards: List[BingoBoard], draws: List[ListSet[Int]]): Int =
    draws match
      case draw :: tail =>
        boards.filterNot(_.isBingo(draw)) match
          case Nil =>
            boards.head.sumExcept(draw) * draw.last
          case remaining =>
            findLastBoardScore(remaining, tail)

  override val part1: Int = // 69579
    draws.view.flatMap {
      draw =>
        boards
          .find(_.isBingo(draw))
          .map(_.sumExcept(draw) * draw.last)
    }.head

  override def part2: Int = //
    findLastBoardScore(boards, draws.toList)
