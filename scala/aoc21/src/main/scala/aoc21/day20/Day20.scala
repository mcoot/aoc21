package aoc21.day20

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable.Set as MutableSet

case class Image(val activePixels: MutableSet[(Int, Int)])

case class Input(algorithm: Array[Boolean], initialImage: Image)


object Parsing:
  val pixel: Parser[Boolean] = (Parser.char('#').map(_ => true)) | (Parser.char('.').map(_ => false))

  val algorithm: Parser[Array[Boolean]] = pixel.rep(512, 512).map(_.toList.toArray)

  val image: Parser[Image] = CommonParsers.lineSeparated(pixel.rep.map(_.toList)).map { rawImage =>
    val s: MutableSet[(Int, Int)] = MutableSet()
    for y <- rawImage.indices do
      for x <- rawImage(y).indices do
        if rawImage(y)(x) then
          s.add((x, y))
    Image(s)
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

