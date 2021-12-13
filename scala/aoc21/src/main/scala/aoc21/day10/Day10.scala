package aoc21.day10

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum Position:
  case Open
  case Close

enum BraceType(val open: Char, val close: Char):
  case Paren extends BraceType('(', ')')
  case Square extends BraceType('[', ']')
  case Curly extends BraceType('{', '}')
  case Arrow extends BraceType('<', '>')


case class Token(braceType: BraceType, pos: Position):
  def toChar = pos match
    case Position.Open => braceType.open
    case Position.Close => braceType.close

object Parsing:
  val token: Parser[Token] = Parser.charIn(List('(', ')', '[', ']', '{', '}', '<', '>')).map {
    case '(' => Token(BraceType.Paren, Position.Open)
    case ')' => Token(BraceType.Paren, Position.Close)
    case '[' => Token(BraceType.Square, Position.Open)
    case ']' => Token(BraceType.Square, Position.Close)
    case '{' => Token(BraceType.Curly, Position.Open)
    case '}' => Token(BraceType.Curly, Position.Close)
    case '<' => Token(BraceType.Arrow, Position.Open)
    case '>' => Token(BraceType.Arrow, Position.Close)
  }

  val tokenLine: Parser[List[Token]] = token.rep.map(_.toList)

  val input: Parser[List[List[Token]]] = CommonParsers.lineSeparated(tokenLine)


def parseToProblem(line: List[Token], stack: List[BraceType] = List()): (Option[Token], List[BraceType]) = line match
  // End of line hit
  case Nil => (None, stack)
  case token :: tail => token match
    case Token(t, Position.Open) => parseToProblem(tail, t :: stack)
    case Token(t, Position.Close) => stack match
      // Empty stack... this would imply a closing with no opening brace, this type of error won't be in the input
      case Nil => ???
      case stackType :: stackTail => if t == stackType then parseToProblem(tail, stackTail) else (Some(Token(t, Position.Close)), stackTail)

def scoreLineIfCorrupt(line: List[Token]): Option[Long] = parseToProblem(line)(0).map {
  case Token(BraceType.Paren, _) => 3L
  case Token(BraceType.Square, _) => 57L
  case Token(BraceType.Curly, _) => 1197L
  case Token(BraceType.Arrow, _) => 25137L
}

def scoreLineIfIncomplete(line: List[Token]): Option[Long] = parseToProblem(line) match
  case (None, stack) =>
    Some(stack.foldLeft(0L) { case (score, braceType) =>
      5L * score + (braceType match
        case BraceType.Paren => 1L
        case BraceType.Square => 2L
        case BraceType.Curly => 3L
        case BraceType.Arrow => 4L)
    })
  case (_, _) => None


object Day10 extends SolutionWithParser[List[List[Token]], Long] {
  override def dayNumber: Int = 10

  override def parser: Parser[List[List[Token]]] = Parsing.input

  override def solvePart1(input: List[List[Token]]): Long = input.map(scoreLineIfCorrupt(_).getOrElse(0L)).sum

  override def solvePart2(input: List[List[Token]]): Long =
    val scores = input.map(scoreLineIfIncomplete(_)).filter(_.isDefined).sorted
    scores(scores.length / 2).getOrElse(0)
}

@main def run = Day10.runSolution
