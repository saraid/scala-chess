package org.aqualgidus.chess.game

import org.aqualgidus.chess.notation.StandardAlgebraicNotation

abstract class Move extends StandardAlgebraicNotation {
  def isValid(state: State): Boolean
  def isHalfMove: Boolean

  def execute(state: State): State
}

case class BasicMove(from: Square, to: Square) extends Move {
  val pieceToMove: Piece = from.occupant.get // From space must be occupied.
  def isCapture: Boolean = to.occupant.isDefined && to.occupant.get.side != pieceToMove.side
  def isHalfMove: Boolean = !isCapture && (from.occupant match {
    case Some(Pawn(_)) => true
    case _ => false
  })

  def isValid(state: State): Boolean = {
    if (from.occupant.isEmpty) false
    else if (pieceToMove.side != state.activeColor) false
    else if (to.occupant.isDefined && !isCapture) false
    else if (to.occupant.exists(_.side == pieceToMove.side)) false
    else if (checksOwnSide) false
    else true
  }

  def checksOwnSide: Boolean = false // TODO

  def execute(state: State): State = {
    assert(isValid(state))

    val newCastlingAvailability = pieceToMove match {
      case King(_) => state.castlingAvailability.remove(side = pieceToMove.side)
      case Rook(rook) => from.file match {
        case "a" => state.castlingAvailability.remove(side = pieceToMove.side, kind = "queenside")
        case "h" => state.castlingAvailability.remove(side = pieceToMove.side, kind = "kingside")
      }
      case _ => state.castlingAvailability
    }
    println(s"$toSAN - ${newCastlingAvailability.toFEN}")

    state.copy(
      activeColor = state.nextColor,
      board = state.board.move(from, to),
      enPassantTarget = None, // always reset this. not sure if this is correct?
      castlingAvailability = newCastlingAvailability,
      halfMoveClock = if (isHalfMove) state.halfMoveClock + 1 else state.halfMoveClock,
      fullMoveNumber = if (state.activeColor == Side.Black) state.fullMoveNumber + 1 else state.fullMoveNumber,
    )
  }

  def toSAN: String = {
    s"${from.position}-${to.position}" // TODO
  }
}