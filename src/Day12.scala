object Day12 extends Base(12):

  private val caves =
    inputLines
      .map(_.split('-').toList)
      .flatMap { case List(x, y) => Iterable(List(x, y), List(y, x)) }
      .groupBy(_.head)
      .map { case (cave, neighbours) => cave -> (neighbours.flatten.toSet - cave) }

  private def paths(cave: String, visited: List[(String, String)], smallVisits: Int): Iterable[List[(String, String)]] =
    caves.getOrElse(cave, Set.empty).flatMap(next => paths((cave, next) :: visited, smallVisits))

  private def paths(visited: List[(String, String)], smallVisits: Int): Iterable[List[(String, String)]] =
    visited match
      case (_, "start") :: _ =>
        Iterable.empty
      case (_, "end") :: _ =>
        Iterable(visited.reverse)
      case (_, current) :: _ if current.head.isUpper =>
        paths(current, visited, smallVisits)
      case (_, current) :: previous =>
        val count = previous.count(_._1 == current)
        if count < smallVisits then
          paths(current, visited, if count == 0 then smallVisits else 1)
        else
          Iterable.empty
      case (_, current) :: previous =>
        Iterable.empty

  override def part1: Int = // 4691
    paths("start", Nil, 1).size

  override def part2: Int = // 140718
    paths("start", Nil, 2).size
