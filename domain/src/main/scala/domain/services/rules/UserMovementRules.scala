package domain.services.rules

import ChessRules._
import domain.model.Board
import domain.services.rules.BasicRules._
import domain.services.rules.CheckRules._
import domain.services.rules.MovementRules._

object UserMovementRules {

  def placePiece(move: Move): Either[MoveRejection, Board] = {
    BoardBasics
      .getPiece(move.board, move.from)
      .map { piece =>
        MovePiece(move.board, move.from, move.to, piece)
      }
      .flatMap { implicit movePiece =>
        for {
          _ <- `Piece cannot move to same coordinate it is located`.validate
          _ <- `Piece cannot move like this`.validate
          _ <- `Piece cannot replace piece from same team`.validate
          _ <- `Cannot move to this square: The king would be in check`.validate
        } yield move.board apply movePiece
      }
  }

}
