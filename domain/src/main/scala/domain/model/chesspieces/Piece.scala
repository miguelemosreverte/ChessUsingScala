package domain.model.chesspieces

import domain.model.chesspieces.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}
import domain.model.chesspieces.rules.ChessRules.Move
import domain.model.chesspieces.rules.UserMovementRules.placePiece

sealed trait Piece
object Piece {
  case object Pawn extends Piece
  case object Horse extends Piece
  case object Rook extends Piece
  case object Tower extends Piece
  case object Queen extends Piece
  case object King extends Piece
}



