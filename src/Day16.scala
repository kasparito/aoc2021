import scala.annotation.tailrec
import scala.collection.mutable

object Day16 extends Base(16):

  // override val inputLines: List[String] = """A0016C880162017C3686B18A3D4780""".stripMargin.split("\n").toList

  private val decoder = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  private val bits = inputLines.head.flatMap(decoder)

  private def parseInt(s: String) = java.lang.Integer.parseInt(s, 2)
  private def parseLong(s: String) = java.lang.Long.parseLong(s, 2)

  private object Packet:
    trait Packet:
      val version: Int
      def versionSum: Int
      def eval: Long

    trait Operation extends Packet:
      def sub: List[Packet]
      protected def evalSub: List[Long] = sub.map(_.eval)
      override def versionSum: Int = version + sub.map(_.versionSum).sum

    case class Literal(version: Int, num: Long) extends Packet:
      override def versionSum: Int = version
      override def eval: Long = num

    case class Sum(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = evalSub.sum

    case class Product(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = evalSub.product

    case class Min(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = evalSub.min

    case class Max(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = evalSub.max

    case class GreaterThan(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = if evalSub(0) > evalSub(1) then 1 else 0

    case class LessThan(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = if evalSub(0) < evalSub(1) then 1 else 0

    case class Equal(version: Int, sub: List[Packet]) extends Operation:
      override def eval: Long = if evalSub(0) == evalSub(1) then 1 else 0

  import Packet._

  private def helper(bits: String, pos: Int): (List[Packet], List[Packet]) =
    bits(pos) match
      case '0' =>
        val newPos = pos + 16
        val size = parseInt(bits.slice(pos, newPos))
        (parsePackets2(bits.slice(newPos, newPos + size)), parsePackets2(bits.drop(newPos + size)))
      case '1' =>
        val size = parseInt(bits.slice(pos + 1, pos + 12))
        parsePackets2(bits.drop(pos + 12)).splitAt(size)

  private def parsePackets2(bits: String): List[Packet] =
    if bits.forall(_ == '0') then
      Nil
    else
      val version = parseInt(bits.take(3))
      var pos = 6
      parseInt(bits.slice(3, 6)) match
        case 0 =>
          val (op, rest) = helper(bits, pos)
          Sum(version, op) :: rest
        case 1 =>
          val (op, rest) = helper(bits, pos)
          Product(version, op) :: rest
        case 2 =>
          val (op, rest) = helper(bits, pos)
          Min(version, op) :: rest
        case 3 =>
          val (op, rest) = helper(bits, pos)
          Max(version, op) :: rest
        case 4 =>
          val value = new StringBuilder()
          while bits(pos) == '1' do
            value.addAll(bits.slice(pos + 1, pos + 5))
            pos += 5
          value.addAll(bits.slice(pos + 1, pos + 5))
          Literal(version, parseLong(value.toString)) :: parsePackets2(bits.drop(pos + 5))
        case 5 =>
          val (op, rest) = helper(bits, pos)
          GreaterThan(version, op) :: rest
        case 6 =>
          val (op, rest) = helper(bits, pos)
          LessThan(version, op) :: rest
        case 7 =>
          val (op, rest) = helper(bits, pos)
          Equal(version, op) :: rest

  override def part1: Long = // 934
    parsePackets2(bits).map(_.versionSum).sum

  override def part2: Long = // ???
    parsePackets2(bits).map(_.eval).sum