package lsp

import org.eclipse.lsp4j.Position

// - Mapping -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Used to map from line * column to offsets and back. */
class PositionMap(input: String):
  private val lines = input.split("\n").foldLeft(Accumulator.empty)(_ add _).result

  def toOffset(position: Position): Int =
    def go(remaining: List[Position], curr: Int, currentLine: Int): Int =
      // Note that this completely fails to take into account the fact that the current line might:
      // - not exist
      // - contain fewer characters than position.getCharacter
      // This is intentional - it means we got out of sync somehow and this is still our best guess.
      if currentLine == position.getLine then curr + position.getCharacter
      else
        remaining match
          case head :: tail => go(tail, curr + head.getCharacter + 1, currentLine + 1)
          case _            => curr
    go(lines, 0, 0)

  def toPosition(offset: Int): Position =
    def go(remaining: List[Position], curr: Int, last: Position): Position = remaining match
      case head :: tail =>
        val newOffset = curr + head.getCharacter

        if newOffset >= offset then Position(head.getLine, offset - curr)
        else go(tail, newOffset + 1, head) // + 1 to account for the line break we skipped.
      case _ => last

    go(lines, 0, Position(0, 0))

// - Internals ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

private case class Accumulator(line: Int, lines: List[Position]):
  def result: List[Position] = lines.reverse

  infix def add(str: String): Accumulator = copy(
    line = line + 1,
    lines = Position(line, str.length) :: lines
  )

private object Accumulator:
  def empty: Accumulator = Accumulator(0, List.empty)
