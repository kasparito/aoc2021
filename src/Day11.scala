object Day11 extends Base(11):

  class Squids:
    private val squid = inputLines.map(_.map(_.toString.toInt).toArray).toArray
    private val indices =
      for
        y <- squid.indices
        x <- squid.head.indices
      yield (x, y)

    private def flash(x: Int, y: Int): Unit =
      squid(y)(x) = 0
      for {
        dx <- x - 1 to x + 1
        dy <- y - 1 to y + 1
        if squid.isDefinedAt(dy) && squid.head.isDefinedAt(dx)
      } yield {
        squid(dy)(dx) match
          case 0 =>
          case level if level >= 9 => flash(dx, dy)
          case level => squid(dy)(dx) = level + 1
      }

    val size: Int = indices.size

    def stepFlashes: Int =
      indices.foreach((x, y) => squid(y)(x) = squid(y)(x) + 1)
      indices.foreach((x, y) => if squid(y)(x) == 10 then flash(x, y))
      indices.count((x, y) => squid(y)(x) == 0)

  override def part1: Int = // 1659
    val squids = new Squids
    (1 to 100).map(_ => squids.stepFlashes).sum

  override def part2: Int = // 227
    val squids = new Squids
    LazyList.from(1).find(step => squids.stepFlashes == squids.size).head
