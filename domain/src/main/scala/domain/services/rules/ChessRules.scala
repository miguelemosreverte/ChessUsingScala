package domain.services.rules

import domain.model.{Board, Coordinate, PlacedPiece}

object ChessRules {
  trait MoveRejection {
    final def validate()(implicit move: MovePiece): Either[MoveRejection, Unit] =
      isValid(move) match {
        case true => Right(())
        case false => Left(this)
      }

    protected def isValid: MovePiece => Boolean
  }

  case class Move(board: Board, from: Coordinate, to: Coordinate)

  case class MovePiece(board: Board, from: Coordinate, to: Coordinate, piece: PlacedPiece)
}
trait ChessRules {
  def rules(implicit move: ChessRules.MovePiece): Either[ChessRules.MoveRejection, Board]
}
