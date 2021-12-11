import scala.collection.immutable.Queue

object Day10 extends Base(10):

  private def score: (List[Char], List[Char]) => Int =
    case (Nil, _) => 0
    case ('(' :: input, stack) => score(input, ')' :: stack)
    case ('[' :: input, stack) => score(input, ']' :: stack)
    case ('{' :: input, stack) => score(input, '}' :: stack)
    case ('<' :: input, stack) => score(input, '>' :: stack)
    case (i :: input, s :: stack) if i == s => score(input, stack)
    case (')' :: _, _) => 3
    case (']' :: _, _) => 57
    case ('}' :: _, _) => 1197
    case ('>' :: _, _) => 25137

  override def part1: Int = // 366027
    inputLines.map(input => score(input.toList, Nil)).sum

  private def score2: (List[Char], List[Char]) => Option[Long] =
    case (Nil, stack) =>
      Some(stack.foldLeft(0L) {
        case (total, char) =>
          val charScore = char match
            case ')' => 1
            case ']' => 2
            case '}' => 3
            case '>' => 4
          total * 5 + charScore
      })
    case ('(' :: input, stack) => score2(input, ')' :: stack)
    case ('[' :: input, stack) => score2(input, ']' :: stack)
    case ('{' :: input, stack) => score2(input, '}' :: stack)
    case ('<' :: input, stack) => score2(input, '>' :: stack)
    case (i :: input, s :: stack) if i == s => score2(input, stack)
    case (')' :: _, _) => None
    case (']' :: _, _) => None
    case ('}' :: _, _) => None
    case ('>' :: _, _) => None

  override def part2: Long = // 1118645287
    val scores = inputLines.flatMap(input => score2(input.toList, Nil)).sorted
    scores(scores.size / 2)
