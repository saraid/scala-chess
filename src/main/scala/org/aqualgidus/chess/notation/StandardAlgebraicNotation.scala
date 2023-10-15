package org.aqualgidus.chess.notation

import org.aqualgidus.chess.game._

import scala.util.matching.Regex

trait StandardAlgebraicNotation {
  def toSAN: String
}

object StandardAlgebraicNotation {
  private val castling: String = "(?<castle>[O0]-[O0](?:-[O0])?)"
  private val promotion: String = "=(?<newRank>[QRNB])"
  private val anyPiece: String = "(?<pieceMoved>[KQRNB]?)"
  private val isCapture: String = "x?"
  private val isCheck: String = "[+#]?"
  private val disambiguation: String = "(?<disambiguationFile>[a-h])?(?<disambiguationRank>[1-8])?"
  private val destination: String = "(?<file>[a-h])(?<rank>[1-8])"

  private val CastlePattern: Regex = List(castling, isCheck).mkString.r
  private val PromotionPattern: Regex = List(disambiguation, destination, promotion, isCheck).mkString.r
  private val MovePattern: Regex = List(anyPiece, disambiguation, isCapture, destination, isCheck).mkString.r

  class AmbiguousPieceToMove(message: String) extends RuntimeException(message)
  class CouldNotFindPieceToMove extends RuntimeException

  def apply(state: State, movetext: String): Move = {
    movetext match {
      case CastlePattern(castle) =>
        castle.replaceAll("[0]", "O") match {
          case "O-O" => Castling(Castling.KingSide)
          case "O-O-O" => Castling(Castling.QueenSide)
        }
      case PromotionPattern(dFile, _dRank, file, rank, newRank) =>
        val candidatePawns: List[Square] = state.board
          .previousRank(rank.toInt, state.activeColor)
          .filter { square =>
            square.occupant.exists {
              case pawn: Pawn => pawn.side == state.activeColor
              case _ => false
            }
          }
          .filter { square => Option(dFile).forall(_ == square.file) }

        if (candidatePawns.size > 1) {
          throw new AmbiguousPieceToMove(candidatePawns.mkString(sep = ", "))
        } else if (candidatePawns.isEmpty) {
          throw new RuntimeException("could not find pawn to promote")
        }

        Promotion(candidatePawns.head, state.board(s"$file$rank"), PromoteTo(newRank))
      case MovePattern(pieceMoved, dFile, dRank, file, rank) =>
        println("is basic move: %s %s %s %s %s".format(Option(pieceMoved), Option(dFile), Option(dRank), file, rank))
        val destination = state.board(s"$file$rank")

        val candidateOrigins = state.board.occupiedSquares
          .filter { square => square.occupant.get.side == state.activeColor}
          .filter { square =>
            Option(pieceMoved).filter(_.nonEmpty).flatMap { pieceType =>
              square.occupant.map {
                case King(_) => pieceType == "K"
                case Queen(_) => pieceType == "Q"
                case Rook(_) => pieceType == "R"
                case Bishop(_) => pieceType == "B"
                case Knight(_) => pieceType == "N"
                case Pawn(_) => pieceType == "P"
              }
            }.getOrElse(true) // no pieceMoved set, so we don't filter out anything
          }
          .filter { square => Option(dFile).forall(_ == square.file) }
          .filter { square => Option(dRank).forall(_.toInt == square.rank) }
          .filter { square => square.occupant.get.canMove(state, from = square, to = destination) }

        if (candidateOrigins.size > 1) {
          throw new AmbiguousPieceToMove(candidateOrigins.mkString(sep = ", "))
        } else if (candidateOrigins.isEmpty) {
          throw new RuntimeException("could not find piece to move")
        }

        BasicMove(candidateOrigins.head, destination)
    }
  }
}

object SANAppTest extends App {
  object SAN {
     def apply(state: State, movetext: String): Move = StandardAlgebraicNotation(state, movetext)
  }

  val game = new Game
  println(SAN(game.state, "e4"))

  // black to promote f2 to queen
  val promotionState = State.fromFEN("8/8/8/8/8/8/5p2/8 b - - 0 1")
  println(SAN(promotionState, "f1=Q"))

  // test ambiguousness
  val ambiguousState = State.fromFEN("8/8/8/8/8/8/5p1p/6R1 b - - 0 1")
  try { SAN(ambiguousState, "f1=Q") } catch {
    case e: StandardAlgebraicNotation.AmbiguousPieceToMove => println("was successfully ambiguous " + e)
  }
  println(SAN(ambiguousState, "ff1=Q"))
}