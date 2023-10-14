package org.aqualgidus.chess

import scala.annotation.tailrec

object Main extends App {

  trait ForsytheEdwardsNotation {
    def toFEN: String
  }

  trait StandardAlgebraicNotation {
    def toSAN: String
  }

  class Game {
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
      def apply(position: String): Square = squares(position)

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
    case class Knight(override val side: Side)extends Piece(side: Side) {
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
        println(s"${toSAN} - ${newCastlingAvailability.toFEN}")

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
        }}
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
    val state = new State(
      activeColor = Side.White,
      board = Board.standard,
      enPassantTarget = None,
      castlingAvailability = CastlingAvailability.all,
      halfMoveClock = 0,
      fullMoveNumber = 1
    )

    class History(history: List[(Option[Move], State)]) {
      def :+(move: Move): History = new History(history :+ (Some(move), move.execute(history.last._2)))
    }
    object History {
      def apply(initial: State, moves: List[Move]): History =
        moves.foldLeft(new History(List((None, initial))))(_ :+ _)
    }
    val history = History(state, List.empty)

  }

  val game = new Game
  val whiteKing = game.King(game.Side.White)
  println(game.state.board.occupy("e1", Some(whiteKing)).toFEN)

  val fenRank = new game.FENRank :+ None :+ None :+ None :+ None :+ Some(whiteKing) :+ None :+ None :+ None
  println(fenRank.toString)

  object FENTests {
    def test(list: List[Option[game.Piece]], expected: String): Unit = {
      val result = list.foldLeft(new game.FENRank)(_ :+ _).toString
      assert(result == expected)
      println(s"$list == $expected")
    }

    def testAll(): Unit = {
      val pawn = game.Pawn(game.Side.White)
      test(List(None, None, Some(pawn)), "2P")
      test(List(Some(pawn), None, None), "P2")
      test(List(None, Some(pawn), None), "1P1")
    }
  }
  FENTests.testAll()

  object BoardTests {
    def testFEN(fen: String): Unit = {
      assert(game.Board.fromFEN(fen).toFEN == fen)
      println(s"$fen : ok")
    }

    def testAll(): Unit = {
      testFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
      testFEN("2r2qk1/r4p1Q/b3pBp1/n3P2P/p2p3R/P5P1/2p2PB1/R5K1")
      testFEN("8/8/4R1p1/2k3p1/1p4P1/1P1b1P2/3K1n2/8")
    }
  }
  BoardTests.testAll()


  // Fool's Mate
  val foolsMate = List(
    game.BasicMove(game.state.board("e2"), game.state.board("f3")),
    game.BasicMove(game.state.board("e7"), game.state.board("e5")),
    game.BasicMove(game.state.board("g2"), game.state.board("g4")),
    game.BasicMove(game.state.board("d8"), game.state.board("h4")),
  )
  println(foolsMate.map(_.toSAN))
  val endState = foolsMate.foldLeft(game.state) { (state, move) => {
    println(state.toFEN)
    move.execute(state)
  } }
  println(endState.toFEN)

}
