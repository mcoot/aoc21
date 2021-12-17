package aoc21.day17

import aoc21.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class TargetArea(x1: Int, x2: Int, y1: Int, y2: Int):
  def inArea(pos: Vec) = pos.x >= x1 && pos.x <= x2 && pos.y >= y1 && pos.y <= y2

case class Vec(x: Int, y: Int):
  def +(that: Vec) = new Vec(x + that.x, y + that.y)

case class Projectile(pos: Vec, velocity: Vec)


object Parsing:
  val range: Parser[(Int, Int)] = (CommonParsers.int <* Parser.string("..")) ~ CommonParsers.int

  val targetAreaDefn: Parser[TargetArea] = for
    _ <- Parser.string("target area: x=")
    xRange <- range
    _ <- Parser.string(", y=")
    yRange <- range
  yield
    TargetArea(xRange(0), xRange(1), yRange(0), yRange(1))


def simulateStep(proj: Projectile): Projectile =
  val newPos = proj.pos + proj.velocity
  val newVelocity = Vec(proj.velocity.x - Math.signum(proj.velocity.x).toInt, proj.velocity.y - 1)
  Projectile(newPos, newVelocity)

def bestYIfHitsTargetArea(initialVelocity: Vec, target: TargetArea): Option[Int] =
  var proj = Projectile(Vec(0, 0), initialVelocity)
  var bestY = Int.MinValue

  // Keep trying while our height is enough that we could hit the target area
  while proj.pos.y >= target.y1 && !target.inArea(proj.pos) do
    proj = simulateStep(proj)
    if proj.pos.y > bestY then
      bestY = proj.pos.y

  if target.inArea(proj.pos) then
    Some(bestY)
  else
    None

// No thoughts head empty
val yRange = -200 to 200

def findBestInitialVelocity(target: TargetArea): Int =
  // We can bound our search at the right bound of the target,
  // since if we project with more horizontal velocity than that we can't possibly hit it
  val maxX = target.x2

  var bestY = Int.MinValue

  for initialX <- 1 to maxX do
    for initialY <- yRange do
      val maybeBestY = bestYIfHitsTargetArea(Vec(initialX, initialY), target)
      if maybeBestY.isDefined && maybeBestY.get > bestY then
        bestY = maybeBestY.get

  bestY

def findNumInitialVelocitiesThatHit(target: TargetArea): Int =
  // We can bound our search at the right bound of the target,
  // since if we project with more horizontal velocity than that we can't possibly hit it
  val maxX = target.x2
  var count = 0

  for initialX <- 1 to maxX do
    for initialY <- yRange do
      val maybeBestY = bestYIfHitsTargetArea(Vec(initialX, initialY), target)
      if maybeBestY.isDefined then
        count += 1

  count


object Day17 extends SolutionWithParser[TargetArea, Int]:
  override def dayNumber: Int = 17

  override def parser: Parser[TargetArea] = Parsing.targetAreaDefn

  override def solvePart1(input: TargetArea): Int = findBestInitialVelocity(input)

  override def solvePart2(input: TargetArea): Int = findNumInitialVelocitiesThatHit(input)


@main def run = Day17.runSolution
