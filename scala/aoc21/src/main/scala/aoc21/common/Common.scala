package aoc21.common

import cats.parse.Parser

import scala.io.Source


extension [K, V] (m: Map[K, V])
  def putIfAbsent(k: K, v: V): Map[K, V] = if m.contains(k) then m else m.updated(k, v)

/**
 * Execute the function and time it in milliseconds
 *
 * @param f function to execute
 * @tparam A return type of the function
 * @return a pair of the result and the time taken
 */
def withTime[A](f: => A): (A, Long) =
  val start = System.nanoTime()
  val result = f
  val end = System.nanoTime()
  (result, (end - start) / 1_000_000)

trait Solution[InputType, OutputType]:
  def dayNumber: Int
  def processInput(rawInput: Source): InputType
  def solvePart1(input: InputType): OutputType
  def solvePart2(input: InputType): OutputType

  private def runAndPrint(source: Source, fs: List[(String, InputType => OutputType)]) =
    val input = processInput(source)
    for (label, f) <- fs do
      println(s"Executing ${label}:")
      val (result, time) = withTime { f(input) }
      println(s"\tResult: ${result} (${time}ms)")

  private def runAndPrintFromFile(filename: String, fs: List[(String, InputType => OutputType)]) =
    runAndPrint(Source.fromFile(filename), fs)

  def testProcessInput(testSuffix: String = "") =
    println(processInput(Source.fromFile(s"./data/test/day${dayNumber}${testSuffix}.in")))

  final def runPart1 = runAndPrintFromFile(
    s"./data/input/day${dayNumber}.in",
    List(("Part 1", solvePart1))
  )

  final def testPart1(testSuffix: String = "") = runAndPrintFromFile(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 1 [TEST]", solvePart1))
  )

  final def runPart2 = runAndPrintFromFile(
    s"./data/input/day${dayNumber}.in",
    List(("Part 2", solvePart2))
  )

  final def testPart2(testSuffix: String = "") = runAndPrintFromFile(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 2 [TEST]", solvePart2))
  )

  final def runSolution = runAndPrintFromFile(
    s"./data/input/day${dayNumber}.in",
    List(("Part 1", solvePart1), ("Part 2", solvePart2))
  )

  final def testSolutionLiteral(test: String) = runAndPrint(
    Source.fromString(test),
    List(("Part 1", solvePart1), ("Part 2", solvePart2))
  )

  final def testSolution(testSuffix: String = "") = runAndPrintFromFile(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 1 [TEST]", solvePart1), ("Part 2 [TEST]", solvePart2))
  )

def yoloParse[T](parser: Parser[T], input: String): T = parser
  .parse(input)
  .map { case ((_, res)) => res }
  .getOrElse {
    throw new Exception("Failed parsing input")
  }

trait SolutionWithParser[InputType, OutputType] extends Solution[InputType, OutputType]:
  def parser: Parser[InputType]

  override def processInput(rawInput: Source): InputType = yoloParse(parser, rawInput.mkString)