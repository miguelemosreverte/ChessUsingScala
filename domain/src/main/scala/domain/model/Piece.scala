package domain.model

sealed trait Piece
object Piece {
  case object Pawn extends Piece
  case object Horse extends Piece
  case object Rook extends Piece
  case object Tower extends Piece
  case object Queen extends Piece
  case object King extends Piece
}
