package org.aqualgidus.chess.game

import org.aqualgidus.chess.notation.{ForsytheEdwardsNotation, StandardAlgebraicNotation => SAN}

class History(history: List[(Option[Move], State)]) {
  def :+(move: Move): History = new History(history :+ (Some(move), move.execute(history.last._2)))
  def lastState: State = history.last._2
  def lastMove: Option[Move] = history.last._1

  def toPGN: String =
    history.tail.grouped(2).map { round =>
      val List(white, black) = round
      val roundIndex = s"${white._2.fullMoveNumber}."
      List(
        roundIndex,
        white._1.get.toSAN(white._2),
        black._1.map(_.toSAN(white._2)).getOrElse("")
      ).filter(_.nonEmpty).mkString(sep = " ")
    }.mkString(sep = "\n")
}

object History {
  def apply(initial: State, moves: List[Move]): History =
    moves.foldLeft(new History(List((None, initial))))(_ :+ _)

  def empty: History = History(State.initial, List.empty)
}
class Game(val history: History) extends ForsytheEdwardsNotation {
  def play(sanMoveText: String): Game = new Game(history :+ SAN(history.lastState, sanMoveText))
  def lastState: State = history.lastState
  def lastMove: Option[Move] = history.lastMove
  def state: State = history.lastState
  def board: Board = state.board
  def toFEN: String = state.toFEN
  def toPGN: String = history.toPGN
}
object Game {
  def initial: Game = new Game(History.empty)
}