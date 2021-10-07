package domain.services.rules

import ChessRules._
import domain.model.Board

object BasicRules extends ChessRules {
  def rules(implicit move: MovePiece): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece cannot move to same coordinate it is located`.validate
      _ <- `Piece cannot replace piece from same team`.validate
    } yield move.board

  case object `Piece cannot move to same coordinate it is located` extends MoveRejection {
    def isValid = {
      case MovePiece(board, from, to, piece) => from != to
    }
  }

  case object `Piece cannot replace piece from same team` extends MoveRejection {
    def isValid = {
      case move @ MovePiece(board, from, to, piece) =>
        board.placedPieces.get(to) match {
          case Some(to) => piece.team != to.team
          case None => true
        }
    }
  }

}
