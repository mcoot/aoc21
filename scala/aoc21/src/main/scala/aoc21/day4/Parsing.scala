package aoc21.day4

import cats.parse._

import aoc21.common.CommonParsers

object Day4Parsing {
  val drawNumbers = CommonParsers.commaSeparated(CommonParsers.int)

  val bingoRow =
    CommonParsers.withTrimmedStartingSpaces(CommonParsers.spaceSeparated(CommonParsers.int))

  val bingoBoard = bingoRow.repSep(5, 5, CommonParsers.newLine)
    .map(_.map(_.toArray).toList.toArray)
    .map(new BingoBoard(_))

  val bingoInput = for
      nums <- drawNumbers <* CommonParsers.newLine
      _ <- CommonParsers.newLine
      boards <- bingoBoard.repSep(1, CommonParsers.newLine ~ CommonParsers.newLine)
    yield
      new BingoInput(nums, boards.toList)
}