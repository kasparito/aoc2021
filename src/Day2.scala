object Day2 extends Base(2):

  enum Command:
    case Up(units: Int)
    case Down(units: Int)
    case Forward(units: Int)

  object Command:

    private val Pattern = """(\w+) (\d+)""".r

    def parse(s: String): Command =
      s match
        case Pattern(name, units) =>
          apply(name, units.toInt)

    private def apply(name: String, units: Int): Command =
      name match
        case "up" => Up(units)
        case "down" => Down(units)
        case "forward" => Forward(units)

  import Command._

  private val commands = inputLines.map(parse)

  override val part1: Int = // 1924923
    val (position, depth) = commands.foldLeft((0, 0)) {
      case ((x, y), Up(units)) => (x, y - units)
      case ((x, y), Down(units)) => (x, y + units)
      case ((x, y), Forward(units)) => (x + units, y)
    }
    position * depth

  override val part2: Int = // 1982495697
    val (position, depth, _) = commands.foldLeft((0, 0, 0)) {
      case ((x, y, aim), Up(units)) => (x, y, aim - units)
      case ((x, y, aim), Down(units)) => (x, y, aim + units)
      case ((x, y, aim), Forward(units)) => (x + units, y + aim * units, aim)
    }
    position * depth
