package aoc21.day4


class BingoBoard(arr: Array[Array[Int]], marked: Array[Array[Boolean]]):
  var bestLine = 0

  def this(arr: Array[Array[Int]]) = this(arr, Array.ofDim[Boolean](arr.length, arr(0).length))

  def copy() = new BingoBoard(arr.clone(), marked.clone())

  def updateBestLine(row: Int, col: Int): Unit =
    val hor = marked(0).indices.map(marked(row)(_)).count(b => b)
    val vert = marked.indices.map(marked(_)(col)).count(b => b)
    val best = Math.max(hor, vert)
    if best > bestLine then
      bestLine = best

  def mark(n: Int) = for
    row <- marked.indices
    col <- marked(0).indices
  do
    if arr(row)(col) == n then
      marked(row)(col) = true
      updateBestLine(row, col)

  def isBingo = bestLine >= 5

  def unmarkedSum = (for
    row <- marked.indices
    col <- marked(0).indices
    if !marked(row)(col)
  yield
    arr(row)(col))
  .sum


case class BingoInput(drawNumbers: List[Int], boards: List[BingoBoard])