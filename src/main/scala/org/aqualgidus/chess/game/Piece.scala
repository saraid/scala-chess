package org.aqualgidus.chess.game

import scala.annotation.tailrec
import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

abstract class Piece(val side: Side) extends ForsytheEdwardsNotation

object Piece {
  def fromFEN(fen: String): Piece = fen match {
    case "K" => King(Side.White)
    case "k" => King(Side.Black)
    case "Q" => Queen(Side.White)
    case "q" => Queen(Side.Black)
    case "R" => Rook(Side.White)
    case "r" => Rook(Side.Black)
    case "B" => Bishop(Side.White)
    case "b" => Bishop(Side.Black)
    case "N" => Knight(Side.White)
    case "n" => Knight(Side.Black)
    case "P" => Pawn(Side.White)
    case "p" => Pawn(Side.Black)
  }
}

case class King(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "K"
    case Side.Black => "k"
  }
}

case class Queen(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "Q"
    case Side.Black => "q"
  }
}

case class Rook(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "R"
    case Side.Black => "r"
  }
}

case class Bishop(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "B"
    case Side.Black => "b"
  }
}

case class Knight(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "N"
    case Side.Black => "n"
  }
}

case class Pawn(override val side: Side) extends Piece(side: Side) {
  def toFEN: String = side match {
    case Side.White => "P"
    case Side.Black => "p"
  }
}