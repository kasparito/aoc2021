object Day9 extends Base(9):

  private val grid = inputLines.map(_.map(_.toString.toInt)).toIndexedSeq

  private def riskLevel(x: Int, y: Int, height: Int): Int =
    val minAdjacent = Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      .collect { case (x, y) if grid.isDefinedAt(y) && grid(y).isDefinedAt(x) => grid(y)(x) }
      .min
    if minAdjacent <= height then 0 else height + 1

  override def part1: Int = // 564
    {
      for {
        (row, y) <- grid.zipWithIndex
        (height, x) <- row.zipWithIndex
      } yield riskLevel(x, y, height)
    }.sum

  private def isAdjacent(p1: (Int, Int), p2: (Int, Int)): Boolean =
    (p1._1 == p2._1 && math.abs(p1._2 - p2._2) == 1) ||
      (p1._2 == p2._2 && math.abs(p1._1 - p2._1) == 1)

  override def part2: Int = // 1038240
    val fields = for {
      (row, y) <- grid.zipWithIndex
      (height, x) <- row.zipWithIndex
      if height != 9
    } yield (x, y)
    fields
      .foldLeft(List.empty[Set[(Int, Int)]]) {
        case (acc, p) =>
          val (mergeFields, otherFields) = acc.partition(_.exists(isAdjacent(_, p)))
          (mergeFields.flatten.toSet + p) :: otherFields
      }
      .map(_.size)
      .sorted
      .takeRight(3)
      .product
