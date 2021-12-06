package aoc21.common

import cats.parse.{Numbers, Parser}

object CommonParsers:
  val spaces = Parser.char(' ').rep

  val newLine = Parser.char('\r').? ~ Parser.char('\n')

  val int = Numbers.digits.map(_.toInt)

  def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
    p.repSep(1, Parser.char(',')).map(_.toList)

  def spaceSeparated[A](p: Parser[A]): Parser[List[A]] =
    p.repSep(1, spaces).map(_.toList)

  def withTrimmedStartingSpaces[A](p: Parser[A]) =
    (spaces.?).with1 *> p

  def lineSeparated[A](p: Parser[A]): Parser[List[A]] =
    p.repSep(1, newLine).map(_.toList)