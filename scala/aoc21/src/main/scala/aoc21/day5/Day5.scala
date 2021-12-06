package aoc21.day5

import cats.parse.{Numbers, Parser}
import aoc21.common.{Solution, SolutionWithParser}

import scala.io.Source
import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.Set as MutableSet

case class LineSegment(start: (Int, Int), end: (Int, Int))

class Board():
  val points: MutableMap[(Int, Int), Int] = MutableMap()
  var intersections = 0

  def addLine(seg: LineSegment, countDiagonals: Boolean) = seg match {
    case LineSegment((x1, y1), (x2, y2)) =>
      val minX = Math.min(x1, x2)
      val minY = Math.min(y1, y2)
      val maxX = Math.max(x1, x2)
      val maxY = Math.max(y1, y2)
      if minX == maxX then
        // Vertical
        points.addAll((minY to maxY).map { y =>
          val curVal = points.getOrElse((minX, y), 0)
          if curVal == 1 then
            intersections += 1
          ((minX, y), curVal + 1)
        })
      else if minY == maxY then
        // Horizontal
        points.addAll((minX to maxX).map { x =>
          val curVal = points.getOrElse((x, minY), 0)
          if curVal == 1 then
            intersections += 1
          ((x, minY), curVal + 1)
        })
      else if countDiagonals then
        // Diagonal
        // 45 degrees only so line length is same hor as vert
        val dist = maxX - minX
        val xDir = if x1 > x2 then -1 else 1
        val yDir = if y1 > y2 then -1 else 1
        points.addAll((0 to dist).map { offset =>
          val (x, y) = (x1 + offset * xDir, y1 + offset * yDir)
          val curVal = points.getOrElse((x, y), 0)
          if curVal == 1 then
            intersections += 1
          ((x, y), curVal + 1)
        })
  }

object Parsing {
  val newLine = Parser.char('\r').? ~ Parser.char('\n')
  val num = Numbers.digits.map(_.toInt)
  val pair = (num <* Parser.char(',')) ~ num
  val lineSegment = for
    start <- Parsing.pair
    _ <- Parser.string(" -> ")
    end <- Parsing.pair
  yield
    new LineSegment(start, end)
  val lineSegments = lineSegment.repSep(1, newLine).map(_.toList)
}


object Day5 extends SolutionWithParser[List[LineSegment], Int]:
  override def dayNumber: Int = 5

  override def parser: Parser[List[LineSegment]] = Parsing.lineSegments

  override def solvePart1(input: List[LineSegment]): Int =
    val board = Board()
    for seg <- input do
      board.addLine(seg, false)
    board.intersections

  override def solvePart2(input: List[LineSegment]): Int =
    val board = Board()
    for seg <- input do
      board.addLine(seg, true)
    board.intersections

@main def run = Day5.runSolution