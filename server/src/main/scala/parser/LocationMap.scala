package parser

class LocationMap(input: String):

  val lines = input.split("\n").foldLeft(Accumulator.empty)(_ add _).result

  def toLocation(offset: Int): Location =
    def go(remaining: List[Location], curr: Int, last: Location): Location = remaining match
      case head :: tail =>
        val newOffset = curr + head.column

        if newOffset >= offset then head.copy(column = offset - curr)
        else go(tail, newOffset + 1, head) // + 1 to account for the line break.
      case _ => last

    go(lines, 0, Location(0, 0))

private case class Accumulator(line: Int, lines: List[Location]):
  def result: List[Location] = lines.reverse

  infix def add(str: String): Accumulator = copy(
    line = line + 1,
    lines = Location(line, str.length) :: lines
  )

private object Accumulator:
  def empty: Accumulator = Accumulator(0, List.empty)

case class Location(line: Int, column: Int)
