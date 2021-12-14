package aoc21.day14

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import scala.collection.mutable.Map as MutableMap

case class InsertionRule(first: Char, second: Char, insertion: Char)

case class PolymerDetails(template: List[Char], rules: Map[(Char, Char), InsertionRule])


object Parsing:
  val element: Parser[Char] = Parser.charIn('A' to 'Z')

  val polymerTemplate: Parser[List[Char]] = element.rep.map(_.toList)

  val pairInsertionRule: Parser[InsertionRule] = for
    first <- element
    second <- element
    _ <- Parser.string(" -> ")
    insertion <- element
  yield
    InsertionRule(first, second, insertion)

  val polymerDetails: Parser[PolymerDetails] = for
    template <- polymerTemplate
    _ <- CommonParsers.newLine ~ CommonParsers.newLine
    rules <- CommonParsers.lineSeparated(pairInsertionRule)
  yield
    val m = rules.foldLeft[Map[(Char, Char), InsertionRule]](Map()) { case (m, rule) =>
      m.updated((rule.first, rule.second), rule)
    }
    PolymerDetails(template, m)


def runStepNaive(template: List[Char], rules: Map[(Char, Char), InsertionRule]): List[Char] = (template.sliding(2).flatMap { window =>
  if rules.contains((window(0), window(1))) then
    List(window(0), rules((window(0), window(1))).insertion)
  else
    List(window(0))
} ++ List(template.last)).toList

def getFrequenciesNaive(template: List[Char]): Map[Char, Long] =
  template.foldLeft[Map[Char, Long]](Map()) { case (m, c) =>
    m.updated(c, m.getOrElse(c, 0L) + 1L)
  }

def getFrequenciesToNStepsBetween(rules: Map[(Char, Char), InsertionRule], first: Char, second: Char, steps: Int, cache: MutableMap[(Char, Char, Int), Map[Char, Long]]): Map[Char, Long] =
  // Use cached value if we have it
  if (cache.contains((first, second, steps))) then
    return cache((first, second, steps))

  // If no steps list, return empty as we can't recurse further
  if steps == 0 then
    return Map()

  val result: Map[Char, Long] = if rules.contains((first, second)) then
    val toInsert = rules((first, second)).insertion
    // Initialise map of counts with the insertion we are making at this step
    val rMap: MutableMap[Char, Long] = MutableMap((toInsert, 1L))
    // Recurse down (first, insertion) and (insertion, second)
    val leftSubCounts = getFrequenciesToNStepsBetween(rules, first, toInsert, steps - 1, cache)
    val rightSubCounts = getFrequenciesToNStepsBetween(rules, toInsert, second, steps - 1, cache)
    for (c, count) <- leftSubCounts do
      rMap.addOne(c, rMap.getOrElse(c, 0L) + count)
    for (c, count) <- rightSubCounts do
      rMap.addOne(c, rMap.getOrElse(c, 0L) + count)
    rMap.toMap
  else
    // If we have no expansion rule for this pairing, we can't recurse further
    // so there are no letters counted underneath this
    Map()

  cache((first, second, steps)) = result
  result


def buildFrequenciesForPolymer(template: List[Char], rules: Map[(Char, Char), InsertionRule], steps: Int): Map[Char, Long] =
  val freqMap: MutableMap[Char, Long] = MutableMap()

  // Add the base letters from step 0 in
  for c <- template do
    freqMap(c) = freqMap.getOrElse(c, 0L) + 1L

  // Recurse through each pair of letters with memoisation to find counts as rules are applied over the steps
  val cache: MutableMap[(Char, Char, Int), Map[Char, Long]] = MutableMap()
  val subCounts = template.sliding(2).map { l =>
    getFrequenciesToNStepsBetween(rules, l(0), l(1), steps, cache)
  }
  for m <- subCounts do
    for (c, count) <- m do
      freqMap(c) = freqMap.getOrElse(c, 0L) + count

  freqMap.toMap

object Day14 extends SolutionWithParser[PolymerDetails, Long]:
  override def dayNumber: Int = 14

  override def parser: Parser[PolymerDetails] = Parsing.polymerDetails

  override def solvePart1(input: PolymerDetails): Long =
    val finalTemplate = (0 until 10).foldLeft(input.template) { case (template, step) =>
      runStepNaive(template, input.rules)
    }
    val freqs = getFrequenciesNaive(finalTemplate).values.toList.sorted
    freqs.last - freqs(0)

  override def solvePart2(input: PolymerDetails): Long =
    val freqs = buildFrequenciesForPolymer(input.template, input.rules, 40).values.toList.sorted
    freqs.last - freqs(0)


@main def run = Day14.runSolution