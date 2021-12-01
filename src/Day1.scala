object Day1 extends Base(1):

  val inputNumbers: Seq[Int] = inputLines.map(_.toInt)

  def increasing(numbers: Seq[Int]): Boolean =
    numbers match
      case Seq(prev, next) => prev < next
      case _ => false

  override val part1: Int = // 1342
    inputNumbers.sliding(2).count(increasing)

  override val part2: Int = //
    inputNumbers.sliding(3).map(_.sum).sliding(2).count(increasing)
