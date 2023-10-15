package org.aqualgidus.chess.game

class Game {
  class History(history: List[(Option[Move], State)]) {
    def :+(move: Move): History = new History(history :+ (Some(move), move.execute(history.last._2)))
  }

  object History {
    def apply(initial: State, moves: List[Move]): History =
      moves.foldLeft(new History(List((None, initial))))(_ :+ _)
  }

  val state: State = State(
    activeColor = Side.White,
    board = Board.standard,
    enPassantTarget = None,
    castlingAvailability = CastlingAvailability.fromFen("KQkq"),
    halfMoveClock = 0,
    fullMoveNumber = 1
  )
  val history: History = History(state, List.empty)

}
