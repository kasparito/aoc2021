object Day2 extends Base(2):

  private val Pattern = """(\w+) (\d+)""".r

  override val part1: Int = // 1924923
    val (position, depth) = inputLines.foldLeft((0, 0)) {
      case ((x, y), Pattern("forward", steps)) => (x + steps.toInt, y)
      case ((x, y), Pattern("up", steps)) => (x, y - steps.toInt)
      case ((x, y), Pattern("down", steps)) => (x, y + steps.toInt)
    }
    position * depth

  override val part2: Int = // 1982495697
    val (position, depth, _) = inputLines.foldLeft((0, 0, 0)) {
      case ((x, y, aim), Pattern("forward", steps)) => (x + steps.toInt, y + aim * steps.toInt, aim)
      case ((x, y, aim), Pattern("up", steps)) => (x, y, aim - steps.toInt)
      case ((x, y, aim), Pattern("down", steps)) => (x, y, aim + steps.toInt)
    }
    position * depth
