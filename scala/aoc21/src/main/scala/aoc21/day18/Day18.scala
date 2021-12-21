package aoc21.day18

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

sealed abstract class SnailfishNumber(var parent: Option[SnailfishNumber]):
  def copy(newParent: Option[SnailfishNumber]): SnailfishNumber


object SnailfishNumber:
  class Literal(var value: Int, parent: Option[SnailfishNumber]) extends SnailfishNumber(parent):
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[SnailfishNumber.Literal]
        && (obj.asInstanceOf[SnailfishNumber.Literal].value == value)

    override def copy(newParent: Option[SnailfishNumber]): SnailfishNumber = Literal(value, newParent)

    override def toString: String = value.toString

  object Literal:
    def unapply(l: Literal): Option[(Int, Option[SnailfishNumber])] = Some((l.value, l.parent))
    def apply(v: Int, parent: Option[SnailfishNumber]) = new Literal(v, parent)

  class Pair(var left: SnailfishNumber, var right: SnailfishNumber, parent: Option[SnailfishNumber]) extends SnailfishNumber(parent):
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[SnailfishNumber.Pair]
        && obj.asInstanceOf[SnailfishNumber.Pair].left == left
        && obj.asInstanceOf[SnailfishNumber.Pair].right == right

    override def copy(newParent: Option[SnailfishNumber]): SnailfishNumber =
      val copied = Pair(null, null, newParent)
      val leftCopy = left.copy(Some(copied))
      val rightCopy = right.copy(Some(copied))
      copied.left = leftCopy
      copied.right = rightCopy
      copied

    override def toString: String = s"[${left.toString},${right.toString}]"

  object Pair:
    def unapply(p: Pair): Option[(SnailfishNumber, SnailfishNumber, Option[SnailfishNumber])] = Some(p.left, p.right, p.parent)
    def apply(left: SnailfishNumber, right: SnailfishNumber, parent: Option[SnailfishNumber]) = new Pair(left, right, parent)


object Parsing:
  val literal: Parser[SnailfishNumber] = CommonParsers.int.map(SnailfishNumber.Literal(_, None))

  val snailfishNumber: Parser[SnailfishNumber] = for
    _ <- Parser.string("[")
    left <- literal | snailfishNumber
    _ <- Parser.string(",")
    right <- literal | snailfishNumber
    _ <- Parser.string("]")
  yield
    val result = SnailfishNumber.Pair(left, right, None)
    left.parent = Some(result)
    right.parent = Some(result)
    result


def rawAddSn(sn1: SnailfishNumber, sn2: SnailfishNumber): SnailfishNumber = SnailfishNumber.Pair(sn1.copy(None), sn2.copy(None), None)

def explodeDownLeftBias(sn: SnailfishNumber, explValue: Int): SnailfishNumber = sn match
  case SnailfishNumber.Literal(v, parent) =>
    SnailfishNumber.Literal(v + explValue, parent)
  case SnailfishNumber.Pair(l, _, _) =>
    // Explode down the left
    // We must always have a regular number somewhere at the leaf so this always works
    val newLeft = explodeDownLeftBias(l, explValue)
    sn.asInstanceOf[SnailfishNumber.Pair].left = newLeft
    sn

def explodeDownRightBias(sn: SnailfishNumber, explValue: Int): SnailfishNumber = sn match
  case SnailfishNumber.Literal(v, parent) =>
    SnailfishNumber.Literal(v + explValue, parent)
  case SnailfishNumber.Pair(_, r, _) =>
    // Explode down the right
    // We must always have a regular number somewhere at the leaf so this always works
    val newRight = explodeDownRightBias(r, explValue)
    sn.asInstanceOf[SnailfishNumber.Pair].right = newRight
    sn

def performExplodeIfNeededHelper(sn: SnailfishNumber, depth: Int): (Boolean, (Option[Int], Option[Int], Boolean)) = sn match
  case SnailfishNumber.Literal(_, _) => (false, (None, None, false))
  case SnailfishNumber.Pair(l, r, parent) =>
    if depth >= 4 then
      // Only works if we have a pair of literals, our rule doesn't work otherwise per the problem
      (true, (Some(l.asInstanceOf[SnailfishNumber.Literal].value), Some(r.asInstanceOf[SnailfishNumber.Literal].value), true))
    else
      // Try left branch first
      val (leftSucceeded, (explL, explR, directChild)) = performExplodeIfNeededHelper(l, depth + 1)
      if directChild then
        // Replace the left branch with 0
        sn.asInstanceOf[SnailfishNumber.Pair].left = SnailfishNumber.Literal(0, Some(sn))
      if leftSucceeded then
        // Need to explode into the right branch, then pass the left expl value up
        if explR.isDefined then
          sn.asInstanceOf[SnailfishNumber.Pair].right = explodeDownLeftBias(r, explR.get)
        (true, (explL, None, false))
      else
        // Try right branch
        val (rightSucceeded, (explL, explR, directChild)) = performExplodeIfNeededHelper(r, depth + 1)
        if directChild then
          // Replace the right branch with 0
          sn.asInstanceOf[SnailfishNumber.Pair].right = SnailfishNumber.Literal(0, Some(sn))
        if rightSucceeded then
          // Need to explode into the left branch, then pass the right expl value up
          if explL.isDefined then
            sn.asInstanceOf[SnailfishNumber.Pair].left = explodeDownRightBias(l, explL.get)
          (true, (None, explR, false))
        else
          // Nothing to explode in this subtree
          (false, (None, None, false))


def performExplodeIfNeeded(sn: SnailfishNumber): Boolean = performExplodeIfNeededHelper(sn, 0)(0)

def splitLiteral(sn: SnailfishNumber.Literal, parent: Option[SnailfishNumber]): SnailfishNumber =
  val result = SnailfishNumber.Pair(null, null, parent)
  result.left = SnailfishNumber.Literal(Math.floor(sn.value.doubleValue / 2.0d).toInt, Some(result))
  result.right = SnailfishNumber.Literal(Math.ceil(sn.value.doubleValue / 2.0d).toInt, Some(result))
  result

def performSplitIfNeededHelper(sn: SnailfishNumber): (Boolean, Boolean) = sn match
  case SnailfishNumber.Literal(n, parent) =>
    if n >= 10 then
      (true, true)
    else
      (false, false)
  case SnailfishNumber.Pair(l, r, parent) =>
    // Try the left branch first
    val (leftSucceeded, wasDirect) = performSplitIfNeededHelper(l)
    if leftSucceeded then
      if wasDirect then
        // We're the parent of the literal we split, replace it in the tree
        sn.asInstanceOf[SnailfishNumber.Pair].left = splitLiteral(l.asInstanceOf[SnailfishNumber.Literal], Some(sn))
      (true, false)
    else
      // Try the right branch
      val (rightSucceeded, wasDirect) = performSplitIfNeededHelper(r)
      if rightSucceeded then
        if wasDirect then
        // We're the parent of the literal we split, replace it in the tree
          sn.asInstanceOf[SnailfishNumber.Pair].right = splitLiteral(r.asInstanceOf[SnailfishNumber.Literal], Some(sn))
        (true, false)
      else
        (false, false)

def performSplitIfNeeded(sn: SnailfishNumber): Boolean = performSplitIfNeededHelper(sn)(0)

def reduceSn(sn: SnailfishNumber): SnailfishNumber =
  var snCopy = sn.copy(None)
  var didExplode = true
  var didSplit = true
  while didExplode || didSplit do
    didExplode = performExplodeIfNeeded(snCopy)
    if !didExplode then
      didSplit = performSplitIfNeeded(snCopy)
  snCopy

def addSn(sn1: SnailfishNumber, sn2: SnailfishNumber): SnailfishNumber = reduceSn(rawAddSn(sn1, sn2))


def magnitude(sn: SnailfishNumber): Int = sn match
  case SnailfishNumber.Literal(n, _) => n
  case SnailfishNumber.Pair(l, r, _) => magnitude(l) * 3 + magnitude(r) * 2


object Day18 extends SolutionWithParser[List[SnailfishNumber], Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[SnailfishNumber]] = CommonParsers.lineSeparated(Parsing.snailfishNumber)

  override def solvePart1(input: List[SnailfishNumber]): Int =
    val finalSn = input.reduce(addSn)
    magnitude(finalSn)

  override def solvePart2(input: List[SnailfishNumber]): Int =
    val pairsOrdered = input.combinations(2).map { l => (l(0), l(1)) }.toList
    val pairs = pairsOrdered ++ pairsOrdered.map(_.swap)
    pairs.map { case (sn1, sn2) => magnitude(addSn(sn1, sn2)) }.max


@main def run = Day18.runSolution
