package aoc21.day12

import aoc21.common.{CommonParsers, SolutionWithParser, putIfAbsent}
import cats.parse.Parser
import scala.collection.mutable.Map as MutableMap

enum CaveSize:
  case Small
  case Large

case class CaveReference(name: String, size: CaveSize)

case class Cave(name: String, size: CaveSize, adjacencies: Set[CaveReference]):
  def reference = CaveReference(name, size)

case class CaveGraph(start: Cave, end: Cave, map: Map[String, Cave]):
  def getCave(ref: CaveReference) = map(ref.name)

object Parsing {
  val smallCaveRef: Parser[CaveReference] =
    Parser.charIn('a' to 'z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Small))
  val largeCaveRef: Parser[CaveReference] =
    Parser.charIn('A' to 'Z').rep.map(l => CaveReference(l.toList.mkString, CaveSize.Large))
  val caveRef: Parser[CaveReference] = smallCaveRef | largeCaveRef

  val caveAdjacency = (caveRef <* Parser.char('-')) ~ caveRef

  val caveGraphParser: Parser[CaveGraph] = CommonParsers.lineSeparated(caveAdjacency).map { adjacencies =>
    val cavesMap: MutableMap[String, Cave] = MutableMap()

    for
      (c1, c2) <- adjacencies
    do
      val c1Adjacencies = cavesMap.get(c1.name).map(_.adjacencies).getOrElse(Set()) + c2
      val c2Adjacencies = cavesMap.get(c2.name).map(_.adjacencies).getOrElse(Set()) + c1

      cavesMap(c1.name) = Cave(c1.name, c1.size, c1Adjacencies)
      cavesMap(c2.name) = Cave(c2.name, c2.size, c2Adjacencies)

    CaveGraph(cavesMap("start"), cavesMap("end"), cavesMap.toMap)
  }
}

def dfs(g: CaveGraph, current: Cave, visitedSmallCaves: Set[CaveReference]): List[List[CaveReference]] =
  if current.name == g.end.name then
    return List(List(current.reference))

  val adjacenciesToVisit = current.adjacencies.diff(visitedSmallCaves)
  val newVisitedSmallCaves = if current.size == CaveSize.Small then
    visitedSmallCaves + current.reference
  else
    visitedSmallCaves

  adjacenciesToVisit.map { c =>
    dfs(g, g.getCave(c), newVisitedSmallCaves)
  }.toList.flatMap { paths =>
    paths.map(current.reference :: _)
  }

def getAllPaths(g: CaveGraph): List[List[CaveReference]] = dfs(g, g.start, Set())

def dfs2(g: CaveGraph, current: Cave, visitedSmallCaves: Set[CaveReference], hasAlreadyRevisitedASmallCave: Boolean): List[List[CaveReference]] =
  if current.name == g.end.name then
    return List(List(current.reference))

  val normalAdjacencies = current.adjacencies.diff(visitedSmallCaves)
  // Potentially revisitable small caves we are adjacent to, disallowing the start cave
  val visitedSmallAdjacencies = current.adjacencies.intersect(visitedSmallCaves) - g.start.reference

  // if we haven't yet revisited a small cave, we can do so here
  val adjacenciesToVisit = if hasAlreadyRevisitedASmallCave then
    // Can't revisit any small caves
    normalAdjacencies.map((_, true))
  else
    // Could revisit a small cave we're adjacent to
    normalAdjacencies.map((_, false)) ++ visitedSmallAdjacencies.map((_, true))

  val newVisitedSmallCaves = if current.size == CaveSize.Small then
    visitedSmallCaves + current.reference
  else
    visitedSmallCaves

  adjacenciesToVisit.map { case (c, revisitedAlready) =>
    dfs2(g, g.getCave(c), newVisitedSmallCaves, revisitedAlready)
  }.toList.flatMap { paths =>
    paths.map(current.reference :: _)
  }

def getAllPaths2(g: CaveGraph): List[List[CaveReference]] = dfs2(g, g.start, Set(), false)

object Day12 extends SolutionWithParser[CaveGraph, Int] {
  override def dayNumber: Int = 12

  override def parser: Parser[CaveGraph] = Parsing.caveGraphParser

  override def solvePart1(input: CaveGraph): Int =
    getAllPaths(input).length

  override def solvePart2(input: CaveGraph): Int =
    getAllPaths2(input).length
}

@main def run = Day12.runSolution