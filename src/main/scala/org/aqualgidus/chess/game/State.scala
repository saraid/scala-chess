package org.aqualgidus.chess.game

import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

class CastlingAvailability(value: Set[(Side, String)]) extends ForsytheEdwardsNotation {
  def remove(side: Side): CastlingAvailability = value.filter(_._1 == side).foldLeft(this)(_ remove _)
  def remove(kind: String): CastlingAvailability = value.filter(_._2 == kind).foldLeft(this)(_ remove _)
  def remove(side: Side, kind: String): CastlingAvailability = remove((side, kind))
  def remove(ca: (Side, String)): CastlingAvailability = new CastlingAvailability(value - ca)

  def toFEN: String = {
    Map(
      (Side.White, "kingside") -> "K",
      (Side.White, "queenside") -> "Q",
      (Side.Black, "kingside") -> "k",
      (Side.Black, "queenside") -> "q",
    ).foldLeft("") { (result, tuple) => {
      if (value(tuple._1)) result + tuple._2
      else result
    }
    }
  }
}

object CastlingAvailability {
  def all: CastlingAvailability = new CastlingAvailability(Set(
    (Side.White, "kingside"),
    (Side.White, "queenside"),
    (Side.Black, "kingside"),
    (Side.Black, "queenside"),
  ))
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