package org.aqualgidus.chess.game

import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

class CastlingAvailability(value: Set[(Side, CastleKind)]) extends ForsytheEdwardsNotation {
  def remove(side: Side): CastlingAvailability = value.filter(_._1 == side).foldLeft(this)(_ remove _)
  def remove(kind: CastleKind): CastlingAvailability = value.filter(_._2 == kind).foldLeft(this)(_ remove _)
  def remove(side: Side, kind: CastleKind): CastlingAvailability = remove((side, kind))
  def remove(ca: (Side, CastleKind)): CastlingAvailability = new CastlingAvailability(value - ca)

  def toFEN: String = {
    Map(
      (Side.White, Castling.KingSide) -> "K",
      (Side.White, Castling.QueenSide) -> "Q",
      (Side.Black, Castling.KingSide) -> "k",
      (Side.Black, Castling.QueenSide) -> "q",
    ).foldLeft("") { (result, tuple) => {
      if (value(tuple._1)) result + tuple._2
      else result
    }
    }
  }
}

object CastlingAvailability {
  def fromFen(code: String): CastlingAvailability = {
    code match {
      case "-" => new CastlingAvailability(Set.empty)
      case _: String => new CastlingAvailability(code.split("").toList.map {
        case "K" => (Side.White, Castling.KingSide)
        case "Q" => (Side.White, Castling.QueenSide)
        case "k" => (Side.Black, Castling.KingSide)
        case "q" => (Side.Black, Castling.QueenSide)
      }.toSet)
    }
  }
}

case class State(
  activeColor: Side,
  board: Board,
  enPassantTarget: Option[Square],
  castlingAvailability: CastlingAvailability,
  halfMoveClock: Int,
  fullMoveNumber: Int
) extends ForsytheEdwardsNotation {
  def nextColor: Side = List(Side.White, Side.Black).filter(_ != activeColor).head

  def toFEN: String =
    List(
      board.toFEN,
      if (activeColor == Side.White) "w" else "b",
      castlingAvailability.toFEN,
      enPassantTarget.map(_.position).getOrElse("-"),
      halfMoveClock.toString,
      fullMoveNumber.toString
    ).mkString(sep = " ")
}
object State {
  def fromFEN(fen: String): State = {
    val Array(
      boardFen: String,
      activeColor: String,
      enPassantFen: String,
      castlingAvailability: String,
      halfMoveClock: String,
      fullMoveNumber: String
    ) = fen.split(" ", 6)

    val board = Board.fromFEN(boardFen)
    val enPassantTarget = enPassantFen match {
      case "-" => None
      case fen: String => board.squares.get(fen)
    }

    new State(
      board = board,
      activeColor = Side.fromFEN(activeColor),
      enPassantTarget = enPassantTarget,
      castlingAvailability = CastlingAvailability.fromFen(castlingAvailability),
      halfMoveClock = halfMoveClock.toInt,
      fullMoveNumber = fullMoveNumber.toInt
    )
  }
}