package org.aqualgidus.chess.game

sealed abstract class Side(name: String) {
  val otherSide: Side
}

object Side {
  final case object White extends Side("white") {
    override val otherSide: Side = Side.Black
  }
  final case object Black extends Side("black") {
    override val otherSide: Side = Side.White
  }
  def fromFEN(string: String): Side = string match {
    case "w" => Side.White
    case "b" => Side.Black
  }
}
