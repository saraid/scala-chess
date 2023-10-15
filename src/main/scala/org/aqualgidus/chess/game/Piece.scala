package org.aqualgidus.chess.game

import scala.annotation.tailrec
import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

trait MoveListFromWalkPath {
  def walkPathForMoveList(walker: Square => Option[Square])(implicit from: Square, side: Side): List[Square] = {
    @tailrec
    def walk(includeSquare: Square => Boolean)(cursor: Option[Square], path: List[Square])(walker: Square => Option[Square]): List[Square] =
      if (cursor.isEmpty) path
      else if (cursor.flatMap(_.occupant).isDefined)
        if (includeSquare(cursor.get)) path :+ cursor.get
        else path
      else walk(includeSquare)(walker(cursor.get), path :+ cursor.get)(walker)

    val squareEmptyOrOccupiedByEnemy: Square => Boolean = (square) => !square.occupant.map(_.side).contains(side)

    walk(squareEmptyOrOccupiedByEnemy)(walker(from), List.empty)(walker)
  }

}

abstract class Piece(val side: Side) extends ForsytheEdwardsNotation {
  def moveList(implicit state: State, from: Square): List[Move] = List.empty
  def canMove(implicit state: State, from: Square, to: Square): Boolean =
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
  override def moveList(implicit state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    val basicMoves = List(
      from.forward.flatMap(_.left),
      from.forward.flatMap(_.right),
      from.backward.flatMap(_.left),
      from.backward.flatMap(_.right),
      from.forward,
      from.backward,
      from.left,
      from.right,
    )
      .flatten
      .map { TypicalMove(from, _) }

    val castlingMoves = List(Castling(Castling.KingSide), Castling(Castling.QueenSide))

    List(basicMoves, castlingMoves)
      .flatten
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "K"
    case Side.Black => "k"
  }
}

case class Queen(override val side: Side) extends Piece(side: Side) with MoveListFromWalkPath {
  override def moveList(implicit state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    List(
      walkPathForMoveList(_.forward),
      walkPathForMoveList(_.backward),
      walkPathForMoveList(_.left),
      walkPathForMoveList(_.right),
      walkPathForMoveList(_.forward.flatMap(_.left)),
      walkPathForMoveList(_.forward.flatMap(_.right)),
      walkPathForMoveList(_.backward.flatMap(_.left)),
      walkPathForMoveList(_.backward.flatMap(_.right)),
    )
      .flatten
      .filterNot(_ == from)
      .map { TypicalMove(from, _) }
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "Q"
    case Side.Black => "q"
  }
}

case class Rook(override val side: Side) extends Piece(side: Side) with MoveListFromWalkPath {
  override def moveList(implicit state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    List(
      walkPathForMoveList(_.forward),
      walkPathForMoveList(_.backward),
      walkPathForMoveList(_.left),
      walkPathForMoveList(_.right),
    )
      .flatten
      .filterNot(_ == from)
      .map { TypicalMove(from, _) }
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "R"
    case Side.Black => "r"
  }
}

case class Bishop(override val side: Side) extends Piece(side: Side) with MoveListFromWalkPath {
  override def moveList(implicit state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    List(
      walkPathForMoveList(_.forward.flatMap(_.left)),
      walkPathForMoveList(_.forward.flatMap(_.right)),
      walkPathForMoveList(_.backward.flatMap(_.left)),
      walkPathForMoveList(_.backward.flatMap(_.right)),
    )
      .flatten
      .filterNot(_ == from)
      .map { TypicalMove(from, _) }
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "B"
    case Side.Black => "b"
  }
}

case class Knight(override val side: Side) extends Piece(side: Side) {
  override def moveList(implicit state: State, from: Square): List[Move] = {
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
      .map { TypicalMove(from, _) }
      .filter(_.isValid(state))
  }
  def toFEN: String = side match {
    case Side.White => "N"
    case Side.Black => "n"
  }
}

case class Pawn(override val side: Side) extends Piece(side) {
  override def moveList(implicit state: State, from: Square): List[Move] = {
    implicit val board: Board = state.board
    implicit val implicitSide: Side = side

    val doubleAdvance: Option[DoubleAdvance] = state.activeColor match {
      case Side.White =>
        val forward1 = from.forward
        val forward2 = forward1.flatMap(_.forward)
        if (List(from.rank == 2, forward1.map(_.occupant).isDefined, forward2.map(_.occupant).isDefined).forall(identity))
          Some(DoubleAdvance(from, forward2.get))
        else None
      case Side.Black =>
        val forward1 = from.forward
        val forward2 = forward1.flatMap(_.forward)
        if (List(from.rank == 7, forward1.map(_.occupant).isDefined, forward2.map(_.occupant).isDefined).forall(identity))
          Some(DoubleAdvance(from, forward2.get))
        else None
    }

    val basicMove = from.forward.map { square => TypicalMove(from, to = square) }
    val captureLeft = from.forward.map(_.left).flatMap { square => {
      square.flatMap(_.occupant) match {
        case Some(Piece(otherSide)) if side != otherSide => Some(TypicalMove(from, square.get))
        case _ => None
      }
    }}
    val captureRight = from.forward.map(_.right).flatMap { square => {
      square.flatMap(_.occupant) match {
        case Some(Piece(otherSide)) if side != otherSide => Some(TypicalMove(from, square.get))
        case _ => None
      }
    }}
    val enPassant = (state.enPassantTarget match {
      case Some(target@Square(_, _, _)) => List(from.left, from.right).flatten.find(_ == target)
      case None => None
    }).map { (square: Square) => EnPassant(from, square.forward.get)}

    def updateToPromotion(simpleMove: Move): List[Move] = {
      val destinationRank = simpleMove.destination.get.rank
      side match {
        case Side.White if destinationRank == 8 => PromoteTo.list.map { Promotion(from, simpleMove.destination.get, _) }
        case Side.Black if destinationRank == 1 => PromoteTo.list.map { Promotion(from, simpleMove.destination.get, _) }
        case _ => List(simpleMove)
      }
    }

    List(basicMove, doubleAdvance, captureLeft, captureRight, enPassant)
      .filter(_.isDefined)
      .map(_.get)
      .flatMap(updateToPromotion(_))
      .filter(_.isValid(state))
  }

  def toFEN: String = side match {
    case Side.White => "P"
    case Side.Black => "p"
  }
}

object PawnTesting  {
  val game = new Game
  val e2 = game.state.board("e2")
  println(e2.occupant)
  val pawn = e2.occupant.get
  println(pawn.moveList(game.state, e2))
}

object KnightTesting  {
  val state = State.fromFEN("8/8/3P4/2p5/4N3/8/8/8 w - - 0 1")
  val e4 = state.board("e4")
  val knight = e4.occupant.get
  val moveList = knight.moveList(state, from = e4)
  println(moveList.map(_.toSAN(state)))
  println(s"move list includes c5 (enemy pawn): ${moveList.map(_.destination).map(_.get).contains(state.board("c5"))}")
  println(s"move list includes d6 (friend pawn): ${moveList.map(_.destination).map(_.get).contains(state.board("d6"))}")

  val corner = State.fromFEN("8/8/8/8/8/8/8/7N w - - 0 1")
  val h1 = corner.board("h1")
  val moveList2 = h1.occupant.get.moveList(corner, h1)
  println(s"corner move list should be size 2: ${moveList2.size}")
}

object RookTesting {
  val state = State.fromFEN("8/8/8/8/4R3/8/8/8 w - - 0 1")
  val e4 = state.board("e4")
  val rook = e4.occupant.get
  val moveList = rook.moveList(state, from = e4)
  println(moveList.map(_.toSAN(state)))
  println(s"Move List should have 14 elements: ${moveList.size}")

  val blockedByEnemy = State.fromFEN("8/4p3/8/8/4R3/8/8/8 w - - 0 1")
  val moveList2 = rook.moveList(blockedByEnemy, from = e4)
  println(moveList2.map(_.toSAN(blockedByEnemy)))
  println(s"Move List should have 13 elements: ${moveList2.size}")
  println(s"Move List should include enemy: ${moveList2.filter(_.destination.get == blockedByEnemy.board("e7"))}")

  val blockedByFriend = State.fromFEN("8/4P3/8/8/4R3/8/8/8 w - - 0 1")
  val moveList3 = rook.moveList(blockedByFriend, from = e4)
  println(moveList3.map(_.toSAN(blockedByFriend)))
  println(s"Move List should have 12 elements: ${moveList3.size}")
  println(s"Move List should exclude friend: ${moveList3.filter(_.destination.get == blockedByFriend.board("e7"))}")

}
object BishopTesting extends App {
  val state = State.fromFEN("8/8/8/8/4B3/8/8/8 w - - 0 1")
  val e4 = state.board("e4")
  val bishop = e4.occupant.get
  val moveList = bishop.moveList(state, from = e4)
  println(moveList.map(_.toSAN(state)))
  println(s"Move List should have 13 elements: ${moveList.size}")

  val blockedByEnemy = State.fromFEN("8/1p6/8/8/4R3/8/8/8 w - - 0 1")
  val moveList2 = bishop.moveList(blockedByEnemy, from = e4)
  println(moveList2.map(_.toSAN(blockedByEnemy)))
  println(s"Move List should have 12 elements: ${moveList2.size}")
  println(s"Move List should include enemy: ${moveList2.filter(_.destination.get == blockedByEnemy.board("b7"))}")

  val blockedByFriend = State.fromFEN("8/1P6/8/8/4R3/8/8/8 w - - 0 1")
  val moveList3 = bishop.moveList(blockedByFriend, from = e4)
  println(moveList3.map(_.toSAN(blockedByFriend)))
  println(s"Move List should have 11 elements: ${moveList3.size}")
  println(s"Move List should exclude friend: ${moveList3.filter(_.destination.get == blockedByFriend.board("b7"))}")

}