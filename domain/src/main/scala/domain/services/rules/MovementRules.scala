package domain.services.rules

import ChessRules._
import domain.model.{Board, Coordinate}
import domain.services.movement.algebra.ChessLanguage
import domain.services.movement.interpreter.ChessLanguageInterpreter

object MovementRules extends ChessRules {
  def rules(implicit move: MovePiece): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece cannot move like this`.validate
    } yield move.board

  case object `Piece cannot move like this` extends MoveRejection {
    def isValid = {
      case MovePiece(board, from, to, piece) =>
        val availableMoves: Set[Coordinate] =
          piece.moves.flatMap(e => ChessLanguageInterpreter.interpret(from, e)(board, piece.team))
        availableMoves contains to

    }
  }

}
