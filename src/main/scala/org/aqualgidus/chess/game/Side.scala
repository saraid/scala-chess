package org.aqualgidus.chess.game

sealed class Side(name: String)

object Side {
  final case object White extends Side("white")
  final case object Black extends Side("black")
  def list: List[Side] = List(Black, White)
  def fromFEN(string: String): Side = string match {
    case "w" => Side.White
    case "b" => Side.Black
  }
}
