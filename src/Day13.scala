object Day13 extends Base(13):

  private val lineIterator = inputLines.iterator

  private val dots = lineIterator.takeWhile(_.nonEmpty).map(_.split(",") match {
    case Array(x, y) => (x.toInt, y.toInt)
  }).toSet

  private val Fold = raw"fold along ([xy])=(\d+)".r
  private val folds = lineIterator.drop(1).map {
    case Fold(axis, line) => (axis, line.toInt)
  }.toSeq

  private def fold: (Set[(Int, Int)], (String, Int)) => Set[(Int, Int)] =
    case (dots, ("x", line)) =>
      dots.collect {
        case (x, y) if x < line => (x, y)
        case (x, y) if x <= 2 * line => (2 * line - x, y)
      }
    case (dots, ("y", line)) =>
      dots.collect {
        case (x, y) if y < line => (x, y)
        case (x, y) if y <= 2 * line => (x, 2 * line - y)
      }

  override def part1: Int = // 847
    fold(dots, folds.head).size

  override def part2: String = // BCZRCEAB
    val remaining = folds.foldLeft(dots)(fold)
    val width = remaining.map(_._1).max + 1
    val height = remaining.map(_._2).max + 1
    val grid = Array.fill(height, width)('.')
    remaining.foreach((x, y) => grid(y)(x) = '#')
    grid.map(new String(_)).mkString("\n", "\n", "\n")
