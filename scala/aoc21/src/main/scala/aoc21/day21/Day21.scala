package aoc21.day21

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.ArrayBuffer as ArrayBuffer


case class PlayerState(pos: Int, score: Long)

case class GameState(p1: PlayerState, p2: PlayerState, diceRolls: Long)


object Parsing:
  val startingPos = (Parser.string("Player ") ~ CommonParsers.int ~ Parser.string(" starting position: ")) *> CommonParsers.int

  val startingPosPair = (startingPos <* CommonParsers.newLine) ~ startingPos


def takeTurn(p: PlayerState, priorDiceRolls: Long): (PlayerState, Long) =
  val rollValue = 3 * priorDiceRolls + 6
  val newPos = (p.pos + rollValue) % 10
  val newScore = p.score + newPos + 1
  (PlayerState(newPos.toInt, newScore), priorDiceRolls + 3)

def runGame(initialPositions: (Int, Int)): GameState = initialPositions match
  case (p1Start, p2Start) =>
    var gs = GameState(PlayerState(p1Start - 1, 0), PlayerState(p2Start - 1, 0), 0)
    var isPlayer2Turn = false
    while gs.p1.score < 1000L && gs.p2.score < 1000L do
      val (newPlayerState, newDicerolls) = takeTurn(if isPlayer2Turn then gs.p2 else gs.p1, gs.diceRolls)
      gs = GameState(if isPlayer2Turn then gs.p1 else newPlayerState, if isPlayer2Turn then newPlayerState else gs.p2, newDicerolls)
      isPlayer2Turn = !isPlayer2Turn
    gs

def finalGameScore(gs: GameState): Long = Math.min(gs.p1.score, gs.p2.score) * gs.diceRolls

def takeTurnQuantum(p: PlayerState): List[PlayerState] =
  val newStates: ArrayBuffer[PlayerState] = ArrayBuffer()
  for roll1 <- 1 to 3 do
    for roll2 <- 1 to 3 do
      for roll3 <- 1 to 3 do
        val newPos = (p.pos + roll1 + roll2 + roll3) % 10
        val newScore = p.score + newPos + 1
        newStates.addOne(PlayerState(newPos, newScore))

  newStates.toList

def execGameQuantum(gs: GameState, cache: MutableMap[(PlayerState, PlayerState), (Long, Long)]): (Long, Long) =
  if cache.contains((gs.p1, gs.p2)) then
    return cache((gs.p1, gs.p2))

  if gs.p1.score >= 100L then
    return (1L, 0L)

  if gs.p2.score >= 100L then
    return (0L, 1L)

  val isPlayer2Turn = gs.diceRolls % 2 == 1
  val possibleNextStates = takeTurnQuantum(if isPlayer2Turn then gs.p2 else gs.p1)

  // Play out sub-games
  val subResults = possibleNextStates.map { pState =>
    val newP1 = if isPlayer2Turn then gs.p1 else pState
    val newP2 = if isPlayer2Turn then pState else gs.p2
    val newGs = GameState(newP1, newP2, gs.diceRolls + 1)
    val result = execGameQuantum(newGs, cache)
    cache((newGs.p1, newGs.p2)) = result
    result
  }

  subResults.foldLeft((0L, 0L)) { case ((p1Wins, p2Wins), (p1New, p2New)) => (p1Wins + p1New, p2Wins + p2New) }

def runGameQuantum(initialPositions: (Int, Int)): (Long, Long) = initialPositions match
  case (p1Start, p2Start) =>
    execGameQuantum(GameState(PlayerState(p1Start - 1, 0), PlayerState(p2Start - 1, 0), 0), MutableMap())


object Day21 extends SolutionWithParser[(Int, Int), Long]:
  override def dayNumber: Int = 21

  override def parser: Parser[(Int, Int)] = Parsing.startingPosPair

  override def solvePart1(input: (Int, Int)): Long =
    val finalState = runGame(input)
    finalGameScore(finalState)

  override def solvePart2(input: (Int, Int)): Long =
    val (p1Victories, p2Victories) = runGameQuantum(input)
    Math.max(p1Victories, p2Victories)


@main def run = Day21.runSolution
