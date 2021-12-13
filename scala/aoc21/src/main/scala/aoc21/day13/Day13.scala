package aoc21.day13

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Dot(x: Int, y: Int)

enum Axis:
  case X
  case Y

case class FoldInstruction(axis: Axis, pos: Int)

case class Page(dots: List[Dot], folds: List[FoldInstruction])

object Parsing {
  val dot = ((CommonParsers.int <* Parser.char(',')) ~ CommonParsers.int).map { case (x, y) => Dot(x, y) }

  val axis = Parser.charIn(List('x', 'y')).map { _ match
    case 'x' => Axis.X
    case 'y' => Axis.Y
  }

  val fold = Parser.string("fold along ") *> ((axis <* Parser.char('=')) ~ CommonParsers.int)
    .map { case (axis, pos) => FoldInstruction(axis, pos) }

  val page = for
    dots <- CommonParsers.lineSeparated(dot)
    _ <- CommonParsers.newLine ~ CommonParsers.newLine
    folds <- CommonParsers.lineSeparated(fold)
  yield
    Page(dots, folds)
}

object Day13 extends SolutionWithParser[Page, Int]:
  override def dayNumber: Int = 13

  override def parser: Parser[Page] = Parsing.page

  override def solvePart1(input: Page): Int =
    println(input)
    ???

  override def solvePart2(input: Page): Int = ???


@main def run = Day13.testSolution()