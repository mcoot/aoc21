package aoc21.day16

import aoc21.common.{SolutionWithParser, yoloParse}
import cats.parse.{Parser, Rfc5234}

enum PacketType:
  case Literal
  case Operator(opCode: Int)

enum PacketMode:
  case BitLengthMode
  case NumPacketsMode

enum Packet(version: Int, packetType: PacketType):
  case Literal(version: Int, value: Int) extends Packet(version, PacketType.Literal)
  case Operator(version: Int, opCode: Int, packets: List[Packet]) extends Packet(version, PacketType.Operator(opCode))


object InputParsing:
  val hexDigitsToBits: Parser[String] = Rfc5234.hexdig.map { c =>
    val intValue = Integer.parseInt(c.toString, 16)
    // It's one hex digit, so only take the last four bits
    // Scala doesn't make this nice lol
    String.format("%04d", intValue.toBinaryString.toInt)
  }.rep.map(_.toList.mkString)


object PacketParsing:
  def bits(length: Int) = Parser.charIn(List('0', '1')).rep(length, length).map(_.toList.mkString)

  def bitsAsInt(length: Int): Parser[Int] = bits(length).map(Integer.parseInt(_, 2))

  val packetVersion: Parser[Int] = bitsAsInt(3)

  val literalPacketType: Parser[PacketType] = Parser.string("100").map(_ => PacketType.Literal)

  val literalPacketSegmentContinue: Parser[Int] = Parser.char('1') *> bitsAsInt(4)

  val literalPacketSegmentEnd: Parser[Int] = Parser.char('0') *> bitsAsInt(4)

  val literalPacketValue: Parser[Int] = (literalPacketSegmentContinue.rep0.with1 ~ literalPacketSegmentEnd).map { case (segs, endSeg) =>
    (segs.foldLeft(0) { case (v, curSeg) => (v << 4) + curSeg } << 4) + endSeg
  }

  val literalPacket: Parser[Packet] = (packetVersion ~ (literalPacketType *> literalPacketValue)).map { case (version, value) =>
    Packet.Literal(version, value)
  }

  val operatorPacketType: Parser[PacketType.Operator] = bitsAsInt(3).map(PacketType.Operator(_))

  val operatorPacketMode: Parser[PacketMode] = bits(1).map {
    case "0" => PacketMode.BitLengthMode
    case "1" => PacketMode.NumPacketsMode
  }

  val operatorPacketBitLength: Parser[Int] = bitsAsInt(15)

  val operatorPacketBitLengthContents: Parser[List[Packet]] = for
    bitLength <- operatorPacketBitLength
  yield
    ???

  val operatorPacketNumPackets: Parser[Int] = bitsAsInt(11)

  val operatorPacketNumPacketsContents: Parser[List[Packet]] = for
    numPackets <- operatorPacketNumPackets
    subPackets <- packet.rep(numPackets, numPackets).map(_.toList)
  yield
    subPackets

  val operatorPacket: Parser[Packet] = for
    version <- packetVersion
    packetType <- operatorPacketType
    mode <- operatorPacketMode
    subPackets <- if mode == PacketMode.BitLengthMode then
      operatorPacketBitLengthContents
    else
      operatorPacketNumPacketsContents
  yield
    Packet.Operator(version, packetType.opCode, subPackets)

  val packet: Parser[Packet] = literalPacket.backtrack | operatorPacket


object Day16 extends SolutionWithParser[String, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[String] = InputParsing.hexDigitsToBits

  override def solvePart1(input: String): Int =
    val parsed = PacketParsing.packet.parse(input)
    println(parsed)
    ???

  override def solvePart2(input: String): Int = ???


@main def run = Day16.testSolution("c")

