package aoc21.day4

import scala.io.Source
import scala.collection.mutable.Set as MutableSet
import aoc21.common.{Solution, SolutionWithParser}

def score(board: BingoBoard, n: Int) = board.unmarkedSum * n

object Day4 extends SolutionWithParser[BingoInput, Int] {
  override def dayNumber: Int = 4

  override def parser = Day4Parsing.bingoInput

  override def solvePart1(input: BingoInput): Int =
    val inputCopy = input.copy()
    for n <- inputCopy.drawNumbers do
      for board <- inputCopy.boards do
        board.mark(n)
        if board.isBingo then
          return score(board, n)
    throw new Exception("No winning board by end of drawn numbers")

  override def solvePart2(input: BingoInput): Int =
    val inputCopy = input.copy()
    val wonBoards: MutableSet[Int] = MutableSet()
    for n <- inputCopy.drawNumbers do
      for
        i <- inputCopy.boards.indices
        if !wonBoards.contains(i)
      do
        val board = inputCopy.boards(i)
        board.mark(n)
        if board.isBingo then
          wonBoards.add(i)
          if wonBoards.size == inputCopy.boards.length then
            return score(board, n)
    throw new Exception("Non-winning boards remained by end of drawn numbers")
}

@main def run =
  Day4.runSolution
