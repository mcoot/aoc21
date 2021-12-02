package aoc21.day2

import aoc21.common.Solution

import scala.io.Source


enum Command:
  case Forward(steps: Int)
  case Up(steps: Int)
  case Down(steps: Int)


val forwardPattern = "forward (\\d+)".r
val upPattern = "up (\\d+)".r
val downPattern = "down (\\d+)".r


object Day2 extends Solution[List[Command], Int]:
  override def dayNumber: Int = 2

  override def processInput(rawInput: Source): List[Command] = rawInput.getLines.map { _ match
    case forwardPattern(n) => Command.Forward(n.toInt)
    case upPattern(n) => Command.Up(n.toInt)
    case downPattern(n) => Command.Down(n.toInt)
  }.toList

  override def solvePart1(input: List[Command]): Int =
    val (pos, depth) = input.foldLeft(0, 0) { case ((pos, depth), command) => command match
      case Command.Forward(n) => (pos + n, depth)
      case Command.Up(n) => (pos, depth - n)
      case Command.Down(n) => (pos, depth + n)
    }
    pos * depth

  override def solvePart2(input: List[Command]): Int =
    val (pos, depth, aim) = input.foldLeft(0, 0, 0) { case ((pos, depth, aim), command) => command match
      case Command.Forward(n) => (pos + n, depth + (n * aim), aim)
      case Command.Up(n) => (pos, depth, aim - n)
      case Command.Down(n) => (pos, depth, aim + n)
    }
    pos * depth


@main def run = Day2.runSolution