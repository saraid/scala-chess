package org.aqualgidus.chess

import scala.annotation.tailrec
import scala.{+:, :+}

object Main extends App {

  trait ForsytheEdwardsNotation {
    def toFEN: String
  }

  class Game extends ForsytheEdwardsNotation {
    class Side(name: String)
    object Side {
      object White extends Side("white")
      object Black extends Side("black")
    }

    class Board extends ForsytheEdwardsNotation {
      val squares: Map[String, Square] = (for {
        file <- List("a", "b", "c", "d", "e", "f", "g", "h")
        rank <- 1 to 8
      } yield (s"$file$rank", Square(rank, file))).toMap

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
        }}.reverse.mkString(sep="/")
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
              println(s"${files.head}$rank: $piece")
              val accumulated = accumulator :+ Square(rank, files.head, Some(Piece.fromFEN(piece)))
              if (remaining.isEmpty) accumulated
              else handleFileFEN(remaining.head, remaining.tail, accumulated, files.tail)
            case EmptySquare(countString) => handleFileFEN((countString.toInt - 1).toString, remaining, accumulator :+ Square(rank, files.head, None), files.tail)
            case "0" => handleFileFEN(remaining.head, remaining.tail, accumulator, files)
          }
          println(s"next rank: $rankFEN")
          val fileFEN = rankFEN.toList.map(_.toString)
          newBoard ++ handleFileFEN(fileFEN.head, fileFEN.tail, List.empty, allFiles)
        }}
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
    }

    abstract class Piece(side: Side) extends ForsytheEdwardsNotation
    object Piece {
      def fromFEN(fen: String): Piece = fen match {
        case "K" => new King(Side.White)
        case "k" => new King(Side.Black)
        case "Q" => new Queen(Side.White)
        case "q" => new Queen(Side.Black)
        case "R" => new Rook(Side.White)
        case "r" => new Rook(Side.Black)
        case "B" => new Bishop(Side.White)
        case "b" => new Bishop(Side.Black)
        case "N" => new Knight(Side.White)
        case "n" => new Knight(Side.Black)
        case "P" => new Pawn(Side.White)
        case "p" => new Pawn(Side.Black)
      }
    }
    class King(side: Side) extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "K"
        case Side.Black => "k"
      }
    }
    class Queen(side: Side) extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "Q"
        case Side.Black => "q"
      }
    }
    class Rook(side: Side) extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "R"
        case Side.Black => "r"
      }
    }
    class Bishop(side: Side) extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "B"
        case Side.Black => "b"
      }
    }
    class Knight (side: Side)extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "N"
        case Side.Black => "n"
      }
    }
    class Pawn(side: Side) extends Piece(side: Side) {
      def toFEN: String = side match {
        case Side.White => "P"
        case Side.Black => "p"
      }
    }

    val board = new Board

    def toFEN: String = {
      board.toFEN // TODO
    }
  }

  val game = new Game
  val whiteKing = new game.King(game.Side.White)
  println(game.board.occupy("e1", Some(whiteKing)).toFEN)

  val fenRank = new game.FENRank :+ None :+ None :+ None :+ None :+ Some(whiteKing) :+ None :+ None :+ None
  println(fenRank.toString)

  object FENTests {
    def test(list: List[Option[game.Piece]], expected: String): Unit = {
      val result = list.foldLeft(new game.FENRank)(_ :+ _).toString
      if (result != expected) {
        println(list)
        println(result)
        throw new AssertionError(s"oh no $list != $expected")
      }
    }

    def testAll(): Unit = {
      val pawn = new game.Pawn(game.Side.White)
      test(List(None, None, Some(pawn)), "2P")
      test(List(Some(pawn), None, None), "P2")
      test(List(None, Some(pawn), None), "1P1")
    }
  }
  FENTests.testAll()

  object BoardTests {
  }
//  println("board" + game.Board.fromFEN("8/8/8/8/4K3/8/8/8").squares.get("e4"))
  println("standard board " + game.Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").toFEN)


}
