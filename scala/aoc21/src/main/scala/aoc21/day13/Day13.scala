package aoc21.day13

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import scala.collection.mutable.StringBuilder

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

def visualise(dots: List[Dot]): String =
  val maxX = dots.maxBy { case Dot(x, y) => x }.x
  val maxY = dots.maxBy { case Dot(x, y) => y }.y
  val arr: Array[Array[Boolean]] = Array.ofDim(maxY + 1, maxX + 1)
  for Dot(x, y) <- dots do
    arr(y).update(x, true)

  val sb = StringBuilder()
  for y <- arr.indices do
    for x <- arr(y).indices do
      sb.addOne(if arr(y)(x) then '#' else '.')
    sb.addOne('\n')

  sb.mkString

def applyFold(dots: List[Dot], fold: FoldInstruction): List[Dot] =
  if fold.axis == Axis.X then
    dots.map {
      case Dot(x, y) if x < fold.pos => Dot(x, y)
      case Dot(x, y) if x > fold.pos => Dot(fold.pos - (x - fold.pos), y)
    }.distinct
  else
    dots.map {
      case Dot(x, y) if y < fold.pos => Dot(x, y)
      case Dot(x, y) if y > fold.pos => Dot(x, fold.pos - (y - fold.pos))
    }.distinct

def getFirstDots(page: Page): List[Dot] = applyFold(page.dots, page.folds(0))

def getFinalDots(page: Page): List[Dot] = page.folds.foldLeft(page.dots) { case (dots, fold) => applyFold(dots, fold) }


object Day13 extends SolutionWithParser[Page, Int]:
  override def dayNumber: Int = 13

  override def parser: Parser[Page] = Parsing.page

  override def solvePart1(input: Page): Int =
    getFirstDots(input).length

  override def solvePart2(input: Page): Int =
    println(visualise(getFinalDots(input)))
    0


@main def run = Day13.runSolution