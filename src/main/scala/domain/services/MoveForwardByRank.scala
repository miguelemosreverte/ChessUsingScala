package domain.services

import domain.model.chessboard.Chessboard
import domain.model.chesspieces.Chesspiece.Team

class MoveForwardByRank(implicit team: Team) extends Move {
  def move(chessboard: Chessboard): Chessboard = {
    val pieceWithTheMinimumRank = chessboard.ownPieces.toSeq.minBy(_.rank)
    Chessboard(
      chessboard.ownPieces.map { piece =>
        piece == pieceWithTheMinimumRank match {
          case true => piece.move(piece.availableMoves.head)
          case false => piece
        }
      } ++ chessboard.enemyPieces
    )
  }
}
