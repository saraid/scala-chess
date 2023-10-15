package org.aqualgidus.chess.game

import scala.annotation.tailrec
import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

abstract class Piece(val side: Side) extends ForsytheEdwardsNotation {
  def moveList(state: State, from: Square): List[Move] = List.empty
  def canMove(state: State, from: Square, to: Square): Boolean =
    moveList(state, from).map(_.destination).filter(_.isDefined).map(_.get).contains(to)
}

object Piece {
  def unapply(piece: Piece): Option[Side] = Some(piece.side)
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
  override def moveList(state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    List(
      from.forward.flatMap(_.forward).flatMap(_.left),
      from.forward.flatMap(_.forward).flatMap(_.right),
      from.backward.flatMap(_.backward).flatMap(_.left),
      from.backward.flatMap(_.backward).flatMap(_.right),
      from.left.flatMap(_.left).flatMap(_.forward),
      from.left.flatMap(_.left).flatMap(_.backward),
      from.right.flatMap(_.right).flatMap(_.forward),
      from.right.flatMap(_.right).flatMap(_.backward),
    )
      .flatten
      .map { BasicMove(from, _) }
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "N"
    case Side.Black => "n"
  }
}

case class Pawn(override val side: Side) extends Piece(side) {
  override def moveList(state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    val doubleAdvance: Option[DoubleAdvance] = state.activeColor match {
      case Side.White => {
        val forward1 = from.forward
        val forward2 = forward1.flatMap(_.forward)
        if (List(from.rank == 2, forward1.map(_.occupant).isDefined, forward2.map(_.occupant).isDefined).forall(identity))
          Some(DoubleAdvance(from, forward2.get))
        else None
      }
      case Side.Black => {
        val forward1 = from.forward
        val forward2 = forward1.flatMap(_.forward)
        if (List(from.rank == 7, forward1.map(_.occupant).isDefined, forward2.map(_.occupant).isDefined).forall(identity))
          Some(DoubleAdvance(from, forward2.get))
        else None
      }
    }

    val basicMove = from.forward.map { square => BasicMove(from, to = square) }
    val captureLeft = from.forward.map(_.left).flatMap { square => {
      square.flatMap(_.occupant) match {
        case Some(Piece(otherSide)) if side != otherSide => Some(BasicMove(from, square.get))
        case _ => None
      }
    }}
    val captureRight = from.forward.map(_.right).flatMap { square => {
      square.flatMap(_.occupant) match {
        case Some(Piece(otherSide)) if side != otherSide => Some(BasicMove(from, square.get))
        case _ => None
      }
    }}

    List(basicMove, doubleAdvance, captureLeft, captureRight)
      .filter(_.isDefined)
      .map(_.get)
      .filter(_.isValid(state))
  }

  def toFEN: String = side match {
    case Side.White => "P"
    case Side.Black => "p"
  }
}

object PawnTesting extends App {
  val game = new Game
  val e2 = game.state.board("e2")
  println(e2.occupant)
  val pawn = e2.occupant.get
  println(pawn.moveList(game.state, e2))
}

object KnightTesting extends App {
  val state = State.fromFEN("8/8/3P4/2p5/4N3/8/8/8 w - - 0 1")
  val e4 = state.board("e4")
  val knight = e4.occupant.get
  val moveList = knight.moveList(state, e4)
  println(moveList.map(_.destination))
  println(s"move list includes c5 (enemy pawn): ${moveList.map(_.destination).map(_.get).contains(state.board("c5"))}")
  println(s"move list includes d6 (friend pawn): ${moveList.map(_.destination).map(_.get).contains(state.board("d6"))}")

  val corner = State.fromFEN("8/8/8/8/8/8/8/7N w - - 0 1")
  val h1 = corner.board("h1")
  val moveList2 = h1.occupant.get.moveList(corner, h1)
  println(s"corner move list should be size 2: ${moveList2.size}")
}