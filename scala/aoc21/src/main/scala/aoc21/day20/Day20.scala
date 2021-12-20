package aoc21.day20

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable.Set as MutableSet

case class Image(activePixels: MutableSet[(Int, Int)], lowerBound: (Int, Int), upperBound: (Int, Int))

case class Input(algorithm: Array[Boolean], initialImage: Image)


object Parsing:
  val pixel: Parser[Boolean] = (Parser.char('#').map(_ => true)) | (Parser.char('.').map(_ => false))

  val algorithm: Parser[Array[Boolean]] = pixel.rep(512, 512).map(_.toList.toArray)

  val image: Parser[Image] = CommonParsers.lineSeparated(pixel.rep.map(_.toList)).map { rawImage =>
    var minX = 0
    var maxX = 0
    var minY = 0
    var maxY = 0
    val s: MutableSet[(Int, Int)] = MutableSet()
    for y <- rawImage.indices do
      for x <- rawImage(y).indices do
        if rawImage(y)(x) then
          minX = Math.min(minX, x)
          maxX = Math.max(maxX, x)
          minY = Math.min(minY, y)
          maxY = Math.max(maxY, y)
          s.add((x, y))
    Image(s, (minX, minY), (maxX, maxY))
  }

  val input: Parser[Input] = for
    alg <- algorithm
    _ <- CommonParsers.newLine ~ CommonParsers.newLine
    img <- image
  yield
    Input(alg, img)


object Day20 extends SolutionWithParser[Input, Int]:
  override def dayNumber: Int = 20

  override def parser: Parser[Input] = Parsing.input

  override def solvePart1(input: Input): Int =
    println(input)
    ???

  override def solvePart2(input: Input): Int = ???


@main def run = Day20.testSolution()

