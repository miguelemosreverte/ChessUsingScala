package domain.model.chesspieces.rules

import domain.model.chesspieces._


object ChessRules {
  case class Move(board: Board, from: Coordinate, to: Coordinate)
  case class MovePiece(board: Board, from: Coordinate, to: Coordinate, piece: PlacedPiece)

  trait MoveRejection {
    protected def isValid: MovePiece => Boolean
    final def validate()(implicit move: MovePiece): Either[MoveRejection, Unit] =
      isValid(move) match {
        case true => Right(())
        case false => Left(this)
      }
  }
}
trait ChessRules {
  def rules(implicit move: ChessRules.MovePiece): Either[ChessRules.MoveRejection, Board]
}