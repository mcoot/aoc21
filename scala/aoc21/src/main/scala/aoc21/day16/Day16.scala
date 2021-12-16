package aoc21.day16

import aoc21.common.{SolutionWithParser, yoloParse}
import cats.parse.{Parser, Rfc5234}

// Have to track the bit lengths myself because fuuuuuuuuuu
case class WithBitLength[A](value: A, length: Int):
  def map[B](f: A => B): WithBitLength[B] = WithBitLength(f(value), length)

  def withAdditionalLength(l: Int) = WithBitLength(value, length + l)

object WithBitLength:
  def fromString(str: String) = WithBitLength(str, str.length)


enum OpCode:
  case Sum
  case Product
  case Min
  case Max
  case GreaterThan
  case LessThan
  case Equals

enum PacketType:
  case Literal
  case Operator(opCode: OpCode)

enum PacketMode:
  case BitLengthMode
  case NumPacketsMode

enum Packet(version: Int, packetType: PacketType):
  case Literal(version: Int, value: Long) extends Packet(version, PacketType.Literal)
  case Operator(version: Int, opCode: OpCode, mode: PacketMode, packets: List[Packet]) extends Packet(version, PacketType.Operator(opCode))


object InputParsing:
  val hexDigitsToBits: Parser[String] = Rfc5234.hexdig.map { c =>
    val intValue = Integer.parseInt(c.toString, 16)
    // It's one hex digit, so only take the last four bits
    // Scala doesn't make this nice lol
    String.format("%04d", intValue.toBinaryString.toInt)
  }.rep.map(_.toList.mkString)


object PacketParsing:
  def bits(length: Int): Parser[WithBitLength[String]] = Parser.charIn(List('0', '1')).rep(length, length).map(l => WithBitLength.fromString(l.toList.mkString))

  def bitsAsInt(length: Int): Parser[WithBitLength[Int]] = bits(length).map(_.map(Integer.parseInt(_, 2)))

  val packetVersion: Parser[WithBitLength[Int]] = bitsAsInt(3)

  val literalPacketType: Parser[WithBitLength[PacketType]] = Parser.string("100").map(_ => WithBitLength(PacketType.Literal, 3))

  val literalPacketSegmentContinue: Parser[WithBitLength[Int]] = Parser.char('1') *> bitsAsInt(4).map(_.withAdditionalLength(1))

  val literalPacketSegmentEnd: Parser[WithBitLength[Int]] = Parser.char('0') *> bitsAsInt(4).map(_.withAdditionalLength(1))

  val literalPacketValue: Parser[WithBitLength[Long]] = (literalPacketSegmentContinue.rep0.with1 ~ literalPacketSegmentEnd).map { case (segs, endSeg) =>
    val value = (segs.foldLeft(0L) { case (v, curSeg) => (v << 4) + curSeg.value } << 4) + endSeg.value
    val length = segs.map(_.length).sum + endSeg.length
    WithBitLength(value, length)
  }

  val literalPacket: Parser[WithBitLength[Packet]] = for
    version <- packetVersion
    packetType <- literalPacketType
    value <- literalPacketValue
  yield
    WithBitLength(Packet.Literal(version.value, value.value), version.length + packetType.length + value.length)

  val opCode: Parser[WithBitLength[OpCode]] = bitsAsInt(3).map(_.map {
    case 0 => OpCode.Sum
    case 1 => OpCode.Product
    case 2 => OpCode.Min
    case 3 => OpCode.Max
    // 4 is literal
    case 5 => OpCode.GreaterThan
    case 6 => OpCode.LessThan
    case 7 => OpCode.Equals
  })

  val operatorPacketType: Parser[WithBitLength[PacketType.Operator]] = opCode.map(_.map(PacketType.Operator(_)))

  val operatorPacketMode: Parser[WithBitLength[PacketMode]] = bits(1).map(_.map {
    case "0" => PacketMode.BitLengthMode
    case "1" => PacketMode.NumPacketsMode
  })

  val operatorPacketBitLength: Parser[WithBitLength[Int]] = bitsAsInt(15)

  def packetsUntilLength(length: Int): Parser[WithBitLength[List[Packet]]] = for
    head <- packet
    tail <- if head.length == length then
      Parser.unit.map(_ => WithBitLength(Nil, 0))
    else
      packetsUntilLength(length - head.length)
  yield
    WithBitLength(head.value :: tail.value, head.length + tail.length)

  val operatorPacketBitLengthContents: Parser[WithBitLength[List[Packet]]] = for
    bitLength <- operatorPacketBitLength
    subPackets <- packetsUntilLength(bitLength.value)
  yield
    WithBitLength(subPackets.value, bitLength.length + subPackets.length)

  val operatorPacketNumPackets: Parser[WithBitLength[Int]] = bitsAsInt(11)

  val operatorPacketNumPacketsContents: Parser[WithBitLength[List[Packet]]] = for
    numPackets <- operatorPacketNumPackets
    subPackets <- packet.rep(numPackets.value, numPackets.value).map(_.toList)
  yield
    WithBitLength(subPackets.map(_.value), numPackets.length + subPackets.map(_.length).sum)

  val operatorPacket: Parser[WithBitLength[Packet]] = for
    version <- packetVersion
    packetType <- operatorPacketType
    mode <- operatorPacketMode
    subPackets <- if mode.value == PacketMode.BitLengthMode then
      operatorPacketBitLengthContents
    else
      operatorPacketNumPacketsContents
  yield
    WithBitLength(Packet.Operator(version.value, packetType.value.opCode, mode.value, subPackets.value), version.length + packetType.length + mode.length + subPackets.length)

  val packet: Parser[WithBitLength[Packet]] = literalPacket.backtrack | operatorPacket


def allVersionNumbers(packet: Packet): Int = packet match
  case Packet.Literal(version, _) => version
  case Packet.Operator(version, _, _, packets) => version + packets.map(allVersionNumbers(_)).sum

def eval(packet: Packet): Long = packet match
  case Packet.Literal(_, value) => value
  case Packet.Operator(_, OpCode.Sum, _, packets) => packets.map(eval(_)).sum
  case Packet.Operator(_, OpCode.Product, _, packets) => packets.map(eval(_)).product
  case Packet.Operator(_, OpCode.Min, _, packets) => packets.map(eval(_)).min
  case Packet.Operator(_, OpCode.Max, _, packets) => packets.map(eval(_)).max
  case Packet.Operator(_, OpCode.GreaterThan, _, packets) =>
    val lhs = eval(packets(0))
    val rhs = eval(packets(1))
    if lhs > rhs then
      1
    else
      0
  case Packet.Operator(_, OpCode.LessThan, _, packets) =>
    val lhs = eval(packets(0))
    val rhs = eval(packets(1))
    if lhs < rhs then
      1
    else
      0
  case Packet.Operator(_, OpCode.Equals, _, packets) =>
    val lhs = eval(packets(0))
    val rhs = eval(packets(1))
    if lhs == rhs then
      1
    else
      0


object Day16 extends SolutionWithParser[String, Long]:
  override def dayNumber: Int = 16

  override def parser: Parser[String] = InputParsing.hexDigitsToBits

  override def solvePart1(input: String): Long =
    val parsed = yoloParse(PacketParsing.packet, input)
    allVersionNumbers(parsed.value)

  override def solvePart2(input: String): Long =
    val parsed = yoloParse(PacketParsing.packet, input)
    eval(parsed.value)


@main def run = Day16.runSolution

