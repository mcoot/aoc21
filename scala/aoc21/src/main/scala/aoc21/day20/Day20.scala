package aoc21.day20

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable
import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.StringBuilder

case class Image(var activePixels: MutableSet[(Int, Int)], var lowerBound: (Int, Int), var upperBound: (Int, Int)):
  def apply(pos: (Int, Int)) = activePixels(pos)

  def inBounds(pos: (Int, Int)) = pos match
    case (x, y) => x >= lowerBound(0) && x <= upperBound(0) && y >= lowerBound(1) && y <= upperBound(1)

  def visualise(): String =
    val sb = StringBuilder()
    val (minX, minY) = lowerBound
    val (maxX, maxY) = upperBound
    for y <- minY to maxY do
      for x <- minX to maxX do
        if activePixels((x, y)) then
          sb.addOne('#')
        else
          sb.addOne('.')
      sb.addOne('\n')
    sb.mkString

case class Enhancer(algorithm: Array[Boolean], var image: Image)


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

  val input: Parser[Enhancer] = for
    alg <- algorithm
    _ <- CommonParsers.newLine ~ CommonParsers.newLine
    img <- image
  yield
    Enhancer(alg, img)


def nextValueForPixel(enhancer: Enhancer, pos: (Int, Int), outsideBoundsOn: Boolean): Boolean =
  val sb = StringBuilder()
  val (x, y) = pos
  for pos <- List((x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)) do
    if enhancer.image(pos) || (!enhancer.image.inBounds(pos) && outsideBoundsOn) then
      sb.addOne('1')
    else
      sb.addOne('0')
  val idx = Integer.parseInt(sb.mkString, 2)
  enhancer.algorithm(idx)


def invert(enhancer: Enhancer) =
  val (minX, minY) = enhancer.image.lowerBound
  val (maxX, maxY) = enhancer.image.upperBound
  val newPixels: MutableSet[(Int, Int)] = MutableSet()
  for x <- minX to maxX do
    for y <- minY to maxY do
      if !enhancer.image((x, y)) then
        newPixels.add((x, y))
  enhancer.image.activePixels = newPixels



def enhanceStep(enhancer: Enhancer, outsideBoundsOn: Boolean) =
  val (minX, minY) = enhancer.image.lowerBound
  val (maxX, maxY) = enhancer.image.upperBound
  val newPixels: MutableSet[(Int, Int)] = MutableSet()
  for x <- minX - 1 to maxX + 1 do
    for y <- minY - 1 to maxY + 1 do
      if nextValueForPixel(enhancer, (x, y), outsideBoundsOn) then
        newPixels.add((x, y))
  enhancer.image = Image(newPixels, (minX - 1, minY - 1), (maxX + 1, maxY + 1))

def totalActivePixels(enhancer: Enhancer) = enhancer.image.activePixels.size


object Day20 extends SolutionWithParser[Enhancer, Int]:
  override def dayNumber: Int = 20

  override def parser: Parser[Enhancer] = Parsing.input

  override def solvePart1(input: Enhancer): Int =
    val doesNeedToFlip = input.algorithm(0) && !input.algorithm(511)
    val inputCopy = Enhancer(input.algorithm, Image(input.image.activePixels.clone(), input.image.lowerBound, input.image.upperBound))
    enhanceStep(inputCopy, false)
    enhanceStep(inputCopy, doesNeedToFlip)
    totalActivePixels(inputCopy)

  override def solvePart2(input: Enhancer): Int =
    val doesNeedToFlip = input.algorithm(0) && !input.algorithm(511)
    val inputCopy = Enhancer(input.algorithm, Image(input.image.activePixels.clone(), input.image.lowerBound, input.image.upperBound))
    for idx <- 1 to 50 do
      enhanceStep(inputCopy, doesNeedToFlip && idx % 2 == 0)
    totalActivePixels(inputCopy)


@main def run = Day20.runSolution

