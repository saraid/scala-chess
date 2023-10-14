package org.aqualgidus.chess

object Main extends App {

  class Game {
    class Side(name: String) {
      object White extends Side("white")
      object Black extends Side("black")
    }
    val board = new Board

    class Board {
      val squares: Map[String, Square] = (for {
        file <- List("a", "b", "c", "d", "e", "f", "g", "h")
        rank <- 1 to 8
      } yield (s"$file$rank", new Square(rank, file))).toMap
    }
    class Square(val rank: Int, val file: String)

    class Piece
    class King extends Piece
    class Queen extends Piece
    class Rook extends Piece
    class Bishop extends Piece
    class Knight extends Piece
    class Pawn extends Piece

  }
}
