package org.aqualgidus.chess.game

import org.aqualgidus.chess.notation.StandardAlgebraicNotation

import scala.annotation.tailrec

abstract class Move extends StandardAlgebraicNotation {
  val destination: Option[Square]
  def isValid(implicit state: State): Boolean
  def isHalfMove: Boolean

  def execute(implicit state: State): State
}

case class TypicalMove(from: Square, to: Square) extends Move {
  override val destination: Option[Square] = Some(to)
  val pieceToMove: Piece = from.occupant.get // From space must be occupied.
  def isCapture: Boolean = to.occupant.isDefined && to.occupant.get.side != pieceToMove.side
  def isHalfMove: Boolean = !isCapture && (from.occupant match {
    case Some(Pawn(_)) => true
    case _ => false
  })

  def isValid(implicit state: State): Boolean = {
    if (from.occupant.isEmpty) false
    else if (pieceToMove.side != state.activeColor) false
    else if (to.occupant.isDefined && !isCapture) false
    else if (to.occupant.exists(_.side == pieceToMove.side)) false
    else if (checksOwnSide) false
    else true
  }

  def enemySide: Side = Side.list.find(_ != pieceToMove.side).get
  def checksOwnSide(implicit state: State): Boolean = placesIntoCheck(state, pieceToMove.side)
  def checksEnemySide(implicit state: State): Boolean = placesIntoCheck(state, enemySide)
  def causesCheckmate(implicit state: State): Boolean = {
    val causesCheck = checksEnemySide(state)
    def kingCannotMove = state.king(enemySide).map { square => square.occupant.get.moveList(state, square) }.get.isEmpty
    causesCheck && kingCannotMove
  }

  def placesIntoCheck(implicit state: State, targetSide: Side): Boolean =
    state.board.occupiedSquares.flatMap { square =>
      square.occupant.map(_.moveList(state, from = square))
    }.flatten.map(_.destination).filter(_.isDefined).map(_.get).contains(state.king(targetSide))

  def enPassantTarget(implicit board: Board, side: Side): Option[Square] = None
  def boardAfterMove(implicit state: State): Board = state.board.move(from, to)
  def execute(implicit state: State): State = {
    assert(isValid(state))

    val newCastlingAvailability = pieceToMove match {
      case King(_) => state.castlingAvailability.remove(side = pieceToMove.side)
      case Rook(rook) => from.file match {
        case "a" => state.castlingAvailability.remove(side = pieceToMove.side, Castling.QueenSide)
        case "h" => state.castlingAvailability.remove(side = pieceToMove.side, Castling.KingSide)
      }
      case _ => state.castlingAvailability
    }

    state.nextState(
      board = boardAfterMove,
      enPassantTarget = enPassantTarget(state.board, pieceToMove.side),
      castlingAvailability = newCastlingAvailability,
      isHalfMove = isHalfMove,
    )
  }

  def toSAN(implicit state: State): String = {
    val destination = to.position
    val pieceMarker = from.occupant.get match {
      case King(_) => "K"
      case Queen(_) => "Q"
      case Rook(_) => "R"
      case Bishop(_) => "B"
      case Knight(_) => "N"
      case _ => "" // pawns go unmarked
    }

    @tailrec
    def buildDisambiguation(phase: String, currentAttempt: String): String = {
      try {
        StandardAlgebraicNotation(state, List(pieceMarker, currentAttempt, destination).flatten.mkString)
        currentAttempt
      } catch {
        case _: StandardAlgebraicNotation.AmbiguousPieceToMove => {
          phase match {
            case "only file" => buildDisambiguation("only rank", from.file)
            case "only rank" => buildDisambiguation("full coord", from.rank.toString)
            case "full coord" => s"${from.file}${from.rank}"
          }
        }
        case _: StandardAlgebraicNotation.CouldNotFindPieceToMove => ""
      }
    }
    val disambiguation = buildDisambiguation("only file", "")

    val captureText =
      if (isCapture) {
        from.occupant.get match {
          case Pawn(_) if disambiguation.isEmpty => s"${from.file}x"
          case _ => "x"
        }
      }
      else ""

    val checkText =
      if (causesCheckmate) "#"
      else if (checksEnemySide) "+"
      else ""

    List(pieceMarker, disambiguation, captureText, destination, checkText)
      .filter(_.nonEmpty).mkString
  }
}

class DoubleAdvance(from: Square, to: Square) extends TypicalMove(from, to) {
  override def enPassantTarget(implicit board: Board, side: Side): Option[Square] = from.forward
}
object DoubleAdvance { def apply(from: Square, to: Square): DoubleAdvance = new DoubleAdvance(from, to) }
class EnPassant(from: Square, to: Square) extends TypicalMove(from, to) {
  // remove the pawn that was captured in passing
  override def boardAfterMove(implicit state: State): Board = super.boardAfterMove.occupy(state.enPassantTarget.get, None)
}
object EnPassant { def apply(from: Square, to: Square): EnPassant = new EnPassant(from, to) }

case class Castling(kind: CastleKind) extends Move {
  override val destination: Option[Square] = None
  private def isImpeded(implicit state: State): Boolean = {
    val possibleImpedences = kind match {
      case Castling.KingSide => state.activeColor match {
        case Side.White => List("f1", "g1")
        case Side.Black => List("f8", "g8")
      }
      case Castling.QueenSide => state.activeColor match {
        case Side.White => List("d1", "c1", "b1")
        case Side.Black => List("d8", "c8", "b8")
      }
    }

    possibleImpedences.map { state.board(_) }.exists(_.occupant.isDefined)
  }

  def isValid(implicit state: State): Boolean = !isImpeded && state.castlingAvailability.contains(state.activeColor, kind)
  def isHalfMove: Boolean = false
  def execute(implicit state: State): State = {
    assert(isValid)
    val rookMovement = (state.activeColor, kind) match {
      case (Side.White, Castling.KingSide) => state.board("h1") -> state.board("f1")
      case (Side.White, Castling.QueenSide) => state.board("a1") -> state.board("c1")
      case (Side.Black, Castling.KingSide) => state.board("h8") -> state.board("f8")
      case (Side.Black, Castling.QueenSide) => state.board("a8") -> state.board("c8")
    }
    val kingPosition = state.activeColor match {
      case Side.White => state.board("e1")
      case Side.Black => state.board("e8")
    }
    val kingTarget = (state.activeColor, kind) match {
      case (Side.White, Castling.KingSide) => state.board("g1")
      case (Side.White, Castling.QueenSide) => state.board("b1")
      case (Side.Black, Castling.KingSide) => state.board("g8")
      case (Side.Black, Castling.QueenSide) => state.board("b8")
    }

    val newBoard = state.board.move(rookMovement._1, rookMovement._2).move(kingPosition, kingTarget)

    state.nextState(
      board = newBoard,
      enPassantTarget = None, // always reset this. not sure if this is correct?
      castlingAvailability = CastlingAvailability.none,
      isHalfMove = isHalfMove
    )
  }
  def toSAN(implicit state: State): String = kind match {
    case Castling.KingSide => "O-O"
    case Castling.QueenSide => "O-O-O"
  }
}
sealed case class CastleKind(name: String)
object Castling {
  final object KingSide extends CastleKind("kingside")
  final object QueenSide extends CastleKind("queenside")
}

case class Promotion(from: Square, to: Square, newRank: PromoteTo) extends Move {
  override val destination: Option[Square] = Some(to)
  def isValid(implicit state: State): Boolean = true
  def isHalfMove: Boolean = false
  def execute(implicit state: State): State = state
  def toSAN(implicit state: State): String = TypicalMove(from, to).toSAN(state)
}
sealed case class PromoteTo(rank: String)
object PromoteTo {
  final object Queen extends PromoteTo("Q")
  final object Rook extends PromoteTo("R")
  final object Bishop extends PromoteTo("B")
  final object Knight extends PromoteTo("N")
  def list: List[PromoteTo] = List(Queen, Rook, Bishop, Knight)
  def apply(string: String): PromoteTo = string match {
    case "Q" => Queen
    case "R" => Rook
    case "B" => Bishop
    case "N" => Knight
  }
}