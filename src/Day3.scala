import scala.annotation.tailrec

object Day3 extends Base(3):

  private val inputNumbers = inputLines

  private def intFromBits(bits: Array[Char]): Int =
    Integer.parseInt(new String(bits), 2)

  private def mostCommonBit(numbers: Seq[String], pos: Int, default: Char): Char =
    numbers.map(_(pos)).foldLeft(0) {
      case (n, char) if char == default => n + 1
      case (n, _) => n - 1
    } match {
      case 0 => default
      case n if n < 0 => '0'
      case n => '1'
    }

  @tailrec
  def findRating(numbers: Seq[String], default: Char, pos: Int = 0): Int =
    if numbers.size == 1 then
      intFromBits(numbers.head.toCharArray)
    else
      val mcb = mostCommonBit(numbers, pos, default)
      findRating(numbers.filter(_(pos) == mcb), default, pos + 1)

  override val part1: Int = // 3277364
    val bits = inputNumbers
      .foldLeft(Array.fill(inputNumbers.head.length)(0)) {
        case (sums, bits) =>
          sums.zip(bits).collect {
            case (n, '0') => n - 1
            case (n, '1') => n + 1
          }
      }
    val gamma = intFromBits(bits.map {
      case n if n > 0 => '1'
      case n => '0'
    })
    val epsilon = intFromBits(bits.map {
      case n if n < 0 => '1'
      case n => '0'
    })
    gamma * epsilon

  override val part2: Int = //
    val oxygenGeneratorRating = findRating(inputNumbers, '1')
    val co2ScrubberRating = findRating(inputNumbers, '0')
    oxygenGeneratorRating * co2ScrubberRating
