package aoc21.day4

import cats.parse._

object Day4Parsing {
  val spaces = Parser.char(' ').rep
  val newLine = Parser.char('\r').? ~ Parser.char('\n')

  val num = Numbers.digits.map(_.toInt)

  val drawNumbers = num.repSep(1, Parser.char(',')).map(_.toList)

  val bingoRow = (spaces.?).with1 *> num.repSep(5, 5, spaces).map(_.toList)

  val bingoBoard = bingoRow.repSep(5, 5, newLine)
    .map(_.map(_.toArray).toList.toArray)
    .map(new BingoBoard(_))

  val bingoInput = for
      nums <- drawNumbers <* newLine
      _ <- newLine
      boards <- bingoBoard.repSep(1, newLine ~ newLine)
    yield
      new BingoInput(nums, boards.toList)
}