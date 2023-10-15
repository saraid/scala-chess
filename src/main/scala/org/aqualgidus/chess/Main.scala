package org.aqualgidus.chess

import org.aqualgidus.chess.game.{State, _}
import org.aqualgidus.chess.notation.{StandardAlgebraicNotation => SAN}

object Main extends App {

  val game = Game.initial
  val whiteKing = King(Side.White)
  println(game.board.occupy("e1", Some(whiteKing)).toFEN)

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
  val foolsMate = List("f3", "e5", "g4", "Qh4")
  val endState = foolsMate.foldLeft(Game.initial) { (game, moveText) => {
    println("%s : %s".format(game.lastMove.map(_.toSAN(game.lastState)), game.lastState.toFEN))
    game.play(moveText)
  } }
  println("%s : %s".format(endState.lastMove.map(_.toSAN(endState.lastState)), endState.lastState.toFEN))

  println(endState.toPGN)
}
