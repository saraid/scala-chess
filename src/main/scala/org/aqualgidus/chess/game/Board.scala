package org.aqualgidus.chess.game

import scala.annotation.tailrec
import org.aqualgidus.chess.notation.ForsytheEdwardsNotation

class Board extends ForsytheEdwardsNotation {
  val squares: Map[String, Square] = (for {
    file <- List("a", "b", "c", "d", "e", "f", "g", "h")
    rank <- 1 to 8
  } yield (s"$file$rank", Square(rank, file))).toMap
  def apply(position: String): Square = squares(position)

  def rank(index: Int): List[Square] = for {
    file <- List("a", "b", "c", "d", "e", "f", "g", "h")
  } yield squares(s"$file$index")
  def previousRank(index: Int, activeSide: Side): List[Square] = activeSide match {
    case Side.White => rank(index - 1)
    case Side.Black => rank(index + 1)
  }
  def occupiedSquares: Map[Square, Piece] = squares.values.foldLeft(Map.empty[Square, Piece]) { (map, square) => {
    square.occupant match {
      case Some(piece) => map + (square -> piece)
      case _ => map
    }
  }}

  def ++(newSquares: List[Square]): Board = newSquares.foldLeft(this) { (newBoard, square) => newBoard.occupy(square) }

  def occupy(square: Square): Board = occupy(square.position, square.occupant)
  def occupy(square: Square, piece: Option[Piece]): Board = occupy(square.position, piece)
  def occupy(target: String, piece: Option[Piece]): Board = {
    val oldBoard = this
    new Board {
      override val squares: Map[String, Square] = oldBoard.squares.map(
        position => (position._1, if (position._2.position == target)
          Square(position._2.rank, position._2.file, piece)
        else
          position._2
        )
      )
    }
  }

  def move(from: Square, to: Square): Board = {
    assert(from.occupant.isDefined)
    occupy(to, from.occupant).occupy(from, None)
  }

  def toFEN: String = {
    (1 to 8).map { rank => {
      List("a", "b", "c", "d", "e", "f", "g", "h")
        .map(file => squares.get(s"$file$rank"))
        .foldLeft(new FENRank)(
          (rankFen: FENRank, square: Option[Square]) =>
            rankFen :+ square.flatMap(_.occupant)
        )
    }
    }.reverse.mkString(sep = "/")
  }
}

object Board {
  def fromFEN(fen: String): Board = {
    val boardFEN = fen.split(" ", 2).head
    var rank = 0
    boardFEN.split("/", 8).reverse.foldLeft(new Board) { (newBoard, rankFEN) => {
      val PieceFEN = "([KkQqRrBbNnPp])".r
      val EmptySquare = "([1-8])".r
      val allFiles = List("a", "b", "c", "d", "e", "f", "g", "h")
      rank += 1

      @tailrec
      def handleFileFEN(fileFEN: String, remaining: List[String], accumulator: List[Square], files: List[String] = allFiles): List[Square] = fileFEN match {
        case "0" if remaining.isEmpty => accumulator
        case PieceFEN(piece) =>
          //              println(s"${files.head}$rank: $piece")
          val accumulated = accumulator :+ Square(rank, files.head, Some(Piece.fromFEN(piece)))
          if (remaining.isEmpty) accumulated
          else handleFileFEN(remaining.head, remaining.tail, accumulated, files.tail)
        case EmptySquare(countString) => handleFileFEN((countString.toInt - 1).toString, remaining, accumulator :+ Square(rank, files.head, None), files.tail)
        case "0" => handleFileFEN(remaining.head, remaining.tail, accumulator, files)
      }

      //          println(s"next rank: $rankFEN")
      val fileFEN = rankFEN.toList.map(_.toString)
      newBoard ++ handleFileFEN(fileFEN.head, fileFEN.tail, List.empty, allFiles)
    }
    }
  }

  def standard: Board = fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
}

class FENRank(value: List[Option[Piece]] = List.empty) {
  def :+(piece: Option[Piece]): FENRank = new FENRank(value :+ piece)

  override def toString: String = {

    @tailrec
    def helper(head: Option[Piece], tail: List[Option[Piece]], accumulator: List[String], emptyCount: Int): List[String] = {
      if (tail.isEmpty && head.isEmpty && emptyCount > 0) accumulator :+ (emptyCount + 1).toString
      else if (tail.isEmpty && head.isDefined && emptyCount > 0) accumulator :+ emptyCount.toString :+ head.map(_.toFEN).get
      else if (tail.isEmpty && head.isDefined) accumulator :+ head.map(_.toFEN).get
      else if (tail.isEmpty) accumulator :+ (emptyCount + 1).toString
      else if (head.isDefined && emptyCount > 0) helper(tail.head, tail.tail, accumulator :+ emptyCount.toString :+ head.map(_.toFEN).get, 0)
      else if (head.isDefined) helper(tail.head, tail.tail, accumulator :+ head.map(_.toFEN).get, 0)
      else helper(tail.head, tail.tail, accumulator, emptyCount + 1)
    }

    helper(value.head, value.tail, List.empty, 0).mkString
  }
}

case class Square(rank: Int, file: String, occupant: Option[Piece] = None) {
  val position = s"$file$rank"

  def forward(implicit board: Board, side: Side): Option[Square] = side match {
    case Side.White => board.squares.get(s"$file${rank + 1}")
    case Side.Black => board.squares.get(s"$file${rank - 1}")
  }

  def backward(implicit board: Board, side: Side): Option[Square] = side match {
    case Side.White => board.squares.get(s"$file${rank - 1}")
    case Side.Black => board.squares.get(s"$file${rank + 1}")
  }

  val allFiles: List[String] = List("a", "b", "c", "d", "e", "f", "g", "h")

  private def leftFile: String = try { allFiles(allFiles.indexOf(file) - 1) } catch {
    case _: IndexOutOfBoundsException => ""
  }
  def left(implicit board: Board): Option[Square] = board.squares.get(s"$leftFile$rank")

  private def rightFile: String = try { allFiles(allFiles.indexOf(file) + 1) } catch {
    case _: IndexOutOfBoundsException => ""
  }
  def right(implicit board: Board): Option[Square] = board.squares.get(s"$rightFile$rank")
}

object SquareTesting extends App {
  implicit val board: Board = Board.standard
  board("e4").left
}