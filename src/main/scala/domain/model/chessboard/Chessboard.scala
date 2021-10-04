package domain.model.chessboard

import domain.model.chesspieces.Chesspiece
import domain.model.chesspieces.Chesspiece.Team

case class Chessboard(chesspieces: Set[Chesspiece]) {
  def ownPieces(implicit team: Team): Set[Chesspiece] = chesspieces.filter(_.team == team)
  def enemyPieces(implicit team: Team): Set[Chesspiece] = chesspieces.filter(_.team != team)

  def chesspiecesByCoordinates: Map[Coordinate, Chesspiece] = chesspieces.toSeq.map{ piece =>
    (piece.coordinate, piece)
  }.toMap

  def movePiece(thisOne: Chesspiece, coordinate: Coordinate) =
    copy(
      chesspieces = (chesspiecesByCoordinates.removed(coordinate) + ((coordinate, thisOne))).values.toSet
    )
}
