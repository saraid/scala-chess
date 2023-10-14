package org.aqualgidus.chess

import org.aqualgidus.chess.game.{State, _}

object Main extends App {
  class Game {
    class History(history: List[(Option[Move], State)]) {
      def :+(move: Move): History = new History(history :+ (Some(move), move.execute(history.last._2)))
    }
    object History {
      def apply(initial: State, moves: List[Move]): History =
        moves.foldLeft(new History(List((None, initial))))(_ :+ _)
    }

    val state = State(
      activeColor = Side.White,
      board = Board.standard,
      enPassantTarget = None,
      castlingAvailability = CastlingAvailability.all,
      halfMoveClock = 0,
      fullMoveNumber = 1
    )
    val history: History = History(state, List.empty)

  }

  val game = new Game
  val whiteKing = King(Side.White)
  println(game.state.board.occupy("e1", Some(whiteKing)).toFEN)

  val fenRank = new FENRank :+ None :+ None :+ None :+ None :+ Some(whiteKing) :+ None :+ None :+ None
  println(fenRank.toString)

  object FENTests {
    def test(list: List[Option[Piece]], expected: String): Unit = {
      val result = list.foldLeft(new FENRank)(_ :+ _).toString
      assert(result == expected)
      println(s"$list == $expected")
    }

    def testAll(): Unit = {
      val pawn = Pawn(Side.White)
      test(List(None, None, Some(pawn)), "2P")
      test(List(Some(pawn), None, None), "P2")
      test(List(None, Some(pawn), None), "1P1")
    }
  }
  FENTests.testAll()

  object BoardTests {
    def testFEN(fen: String): Unit = {
      assert(Board.fromFEN(fen).toFEN == fen)
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
    BasicMove(game.state.board("e2"), game.state.board("f3")),
    BasicMove(game.state.board("e7"), game.state.board("e5")),
    BasicMove(game.state.board("g2"), game.state.board("g4")),
    BasicMove(game.state.board("d8"), game.state.board("h4")),
  )
  println(foolsMate.map(_.toSAN))
  val endState = foolsMate.foldLeft(game.state) { (state, move) => {
    println(state.toFEN)
    move.execute(state)
  } }
  println(endState.toFEN)

}
