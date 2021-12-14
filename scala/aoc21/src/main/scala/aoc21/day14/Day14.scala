package aoc21.day14

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

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


def runStep(template: List[Char], rules: Map[(Char, Char), InsertionRule]): List[Char] = (template.sliding(2).flatMap { window =>
  if rules.contains((window(0), window(1))) then
    List(window(0), rules((window(0), window(1))).insertion)
  else
    List(window(0))
} ++ List(template.last)).toList

def getFrequencies(template: List[Char]): Map[Char, Long] =
  template.foldLeft[Map[Char, Long]](Map()) { case (m, c) =>
    m.updated(c, m.getOrElse(c, 0L) + 1L)
  }

object Day14 extends SolutionWithParser[PolymerDetails, Long]:
  override def dayNumber: Int = 14

  override def parser: Parser[PolymerDetails] = Parsing.polymerDetails

  override def solvePart1(input: PolymerDetails): Long =
    val finalTemplate = (0 until 10).foldLeft(input.template) { case (template, step) =>
      runStep(template, input.rules)
    }
    val freqs = getFrequencies(finalTemplate).values.toList.sorted
    freqs.last - freqs(0)

  override def solvePart2(input: PolymerDetails): Long = ???


@main def run = Day14.testSolution()