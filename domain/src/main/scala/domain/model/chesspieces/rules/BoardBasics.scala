package domain.model.chesspieces.rules

import domain.model.chesspieces.rules.ChessRules._
import domain.model.chesspieces._

object BoardBasics  {

  def getPiece(board: Board, coordinate: Coordinate): Either[`Piece is not in board`.type , PlacedPiece] =
    board.placedPieces.get(coordinate) match {
      case Some(piece) => Right(piece)
      case None => Left(`Piece is not in board`)
    }

  case object `Piece is not in board` extends MoveRejection {
    def isValid = {
      case MovePiece(board, from, to, piece) =>
        board.placedPieces contains from
    }
  }
}