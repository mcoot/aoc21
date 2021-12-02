package aoc21.common

import scala.io.Source

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

  private def runAndPrint(filename: String, fs: List[(String, InputType => OutputType)]) =
    val input = processInput(Source.fromFile(filename))
    for (label, f) <- fs do
      println(s"Executing ${label}:")
      val (result, time) = withTime { f(input) }
      println(s"\tResult: ${result} (${time}ms)")

  def runPart1 = runAndPrint(
    s"./data/input/day${dayNumber}.in",
    List(("Part 1", solvePart1))
  )

  def testPart1(testSuffix: String = "") = runAndPrint(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 1 [TEST]", solvePart1))
  )

  def runPart2 = runAndPrint(
    s"./data/input/day${dayNumber}.in",
    List(("Part 2", solvePart2))
  )

  def testPart2(testSuffix: String = "") = runAndPrint(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 2 [TEST]", solvePart2))
  )

  def runSolution = runAndPrint(
    s"./data/input/day${dayNumber}.in",
    List(("Part 1", solvePart1), ("Part 2", solvePart2))
  )

  def testSolution(testSuffix: String = "") = runAndPrint(
    s"./data/test/day${dayNumber}${testSuffix}.in",
    List(("Part 1 [TEST]", solvePart1), ("Part 2 [TEST]", solvePart2))
  )
