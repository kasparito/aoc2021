import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day8 extends Base(8):

  private val patternLengthDigits = Digit.values.groupBy(_.segments.size).view.mapValues(_.toSet).toMap

  enum Digit(s: String):
    val segments: Set[Char] = s.toSet
    case _0 extends Digit("abcefg")
    case _1 extends Digit("cf")
    case _2 extends Digit("acdeg")
    case _3 extends Digit("acdfg")
    case _4 extends Digit("bcdf")
    case _5 extends Digit("abdfg")
    case _6 extends Digit("abdefg")
    case _7 extends Digit("acf")
    case _8 extends Digit("abcdefg")
    case _9 extends Digit("abcdfg")

  case class DigitPattern(value: String):
    val segments: Set[Char] = value.toSet
    def possibleDigits: Set[Digit] =
      patternLengthDigits(segments.size)

  case class NoteEntry(signalPattern: Seq[DigitPattern], outputValues: Seq[DigitPattern])

  object NoteEntry:
    def parse(entry: String): NoteEntry =
      val digits = entry.split(" ")
      NoteEntry(
        digits.take(10).map(DigitPattern.apply),
        digits.takeRight(4).map(DigitPattern.apply))

  private val noteEntries = inputLines.map(NoteEntry.parse)

  private def decodeDigits(patterns: Seq[DigitPattern]): IndexedSeq[Set[Char]] =
    val digits = Array.fill(10)(Set.empty[Char])
    patterns.sortBy(_.segments.size).foreach { pattern =>
      val digit = pattern match {
        case _ if pattern.possibleDigits.size == 1 =>
          pattern.possibleDigits.head.ordinal
        case _ if pattern.segments.size == 5 =>
          if (pattern.segments -- digits(1)).size == 3 then
            3
          else if (pattern.segments -- digits(4)).size == 2 then
            5
          else
            2
        case _ =>
          if (pattern.segments -- digits(1)).size == 5 then
            6
          else if (pattern.segments -- digits(4)).size == 3 then
            0
          else
            9
      }
      digits(digit) = pattern.segments
    }
    digits.toIndexedSeq

  private def calculateOutputValue(entry: NoteEntry): Int =
    val digits = decodeDigits(entry.signalPattern)
    entry
      .outputValues
      .map(p => digits.indexOf(p.segments))
      .zip(Seq(1000, 100, 10, 1))
      .map(_ * _)
      .sum

  override def part1: Int = // 479
    noteEntries.map(_.outputValues.count(_.possibleDigits.size == 1)).sum

  override def part2: Long = // 1041746
    noteEntries.map(calculateOutputValue).sum
