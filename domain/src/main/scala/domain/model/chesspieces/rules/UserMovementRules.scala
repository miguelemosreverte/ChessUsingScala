package domain.model.chesspieces.rules

import domain.model.chesspieces.rules.ChessRules._
import domain.model.chesspieces._
import domain.model.chesspieces.rules.BasicRules.{`Piece cannot move to same coordinate it is located`, `Piece cannot replace piece from same team`}

object UserMovementRules {

  def placePiece(move: Move)(implicit untouchedPieces: Set[Coordinate]): Either[MoveRejection, Board] = {
    BoardBasics.getPiece(move.board, move.from).map{
      piece => MovePiece(move.board, move.from, move.to, piece)
    }.flatMap{ implicit movePiece =>
      val movementRules = MovementRules.apply()
      import movementRules._
      for {
        _ <- `Piece cannot move to same coordinate it is located`.validate
        _ <- `Piece cannot move like this`.validate
        _ <- `Piece cannot replace piece from same team`.validate
      } yield move.board apply movePiece
    }
  }

}