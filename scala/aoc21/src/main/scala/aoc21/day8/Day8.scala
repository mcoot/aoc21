package aoc21.day8

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import scala.collection.mutable.Map as MutableMap

case class DisplayLine(inputs: List[String], outputs: List[String])

// Automatically sorts the characters in each segment
val segListParser = CommonParsers.spaceSeparated(Parser.charIn('a' to 'z').rep.map(_.toList.sorted.mkString("")))
val displayLineParser = for
  inputs <- segListParser
  _ <- Parser.string(" | ")
  outputs <- segListParser
yield
  DisplayLine(inputs, outputs)


def getDigitMapping(inputs: List[String]): Map[String, Int] =
  val inputsSets = inputs.map(Set(_:_*)).toList

  val digitMap: MutableMap[Int, Set[Char]] = MutableMap()
  // We can get 1, 4, 7, 8 based on length
  digitMap(1) = inputsSets.find(_.size == 2).orNull
  digitMap(4) = inputsSets.find(_.size == 4).orNull
  digitMap(7) = inputsSets.find(_.size == 3).orNull
  digitMap(8) = inputsSets.find(_.size == 7).orNull

  // 1 contains c and f
  // 0, 6 and 9 all have 6 lit segments
  // 6 is the only one which doesn't have the c seg active, so we can find that by intersecting with 1
  digitMap(6) = inputsSets.find { segs =>
    segs.size == 6 &&
      segs.intersect(digitMap(1)).size == 1
  }.orNull

  // 9 contains the d-segment, but 0 doesn't, otherwise they contain the other parts of 4, so can find both of those
  digitMap(0) = inputsSets.find { segs =>
    segs.size == 6 &&
      segs != digitMap(6) &&
      segs.intersect(digitMap(4)).size == 3
  }.orNull
  digitMap(9) = inputsSets.find { segs =>
    segs.size == 6 &&
      segs != digitMap(6) &&
      segs != digitMap(0)
  }.orNull

  // 2, 3, 4 all have five segs lit up
  // Intersection of 3 and 1 has two elements, but the intersection of 2/5 with 1 has only one
  digitMap(3) = inputsSets.find { segs =>
    segs.size == 5 &&
      segs.intersect(digitMap(1)).size == 2
  }.orNull

  // Intersection of 5 and 6 is five segments, but intersection of 2 and 6 is four
  digitMap(2) = inputsSets.find { segs =>
    segs.size == 5 &&
      segs != digitMap(3) &&
      segs.intersect(digitMap(6)).size == 4
  }.orNull
  digitMap(5) = inputsSets.find { segs =>
    segs.size == 5 &&
      segs != digitMap(3) &&
      segs != digitMap(2)
  }.orNull

  digitMap.map{ case (digit, segSet) => (segSet.toList.sorted.mkString(""), digit) }.toMap

def calculateOutput(mapping: Map[String, Int], outputs: List[String]): Int = outputs
  .foldLeft(0) { case (n, s) =>
    n * 10 + mapping(s)
  }


def getOutputValues(lines: List[DisplayLine]): List[Int] = lines.map { line =>
    val mapping = getDigitMapping(line.inputs)
    calculateOutput(mapping, line.outputs)
  }

object Day8 extends SolutionWithParser[List[DisplayLine], Int] {
  override def dayNumber: Int = 8

  override def parser: Parser[List[DisplayLine]] = CommonParsers.lineSeparated(displayLineParser)

  override def solvePart1(input: List[DisplayLine]): Int =
    var count = 0
    for line <- input do
      val map: MutableMap[String, Int] = MutableMap()
      for seg <- line.inputs do
        if seg.length == 2 then
          // Digit 1
          map(seg) = 1
        else if seg.length == 4 then
          // Digit 4
          map(seg) = 4
        else if seg.length == 3 then
          // Digit 7
          map(seg) = 7
        else if seg.length == 7 then
          // Digit 8
          map(seg) = 8
      count += line.outputs.count(map.contains(_))
    count

  override def solvePart2(input: List[DisplayLine]): Int = getOutputValues(input).sum

}

@main def run = Day8.runSolution
