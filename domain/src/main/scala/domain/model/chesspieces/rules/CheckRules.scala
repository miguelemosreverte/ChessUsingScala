package domain.model.chesspieces.rules

import domain.model.chesspieces.Piece.King
import domain.model.chesspieces._
import domain.model.chesspieces.rules.ChessRules._

object CheckRules {

  def `the King is in check`(board: Board, team: Team)(implicit untouchedPieces: Set[Coordinate]) = {
      val (kingPosition, king) = board.placedPieces
        .filter(_._2.piece == King)
        .filter(_._2.team == team)
        .toSeq.head

      val enemies = board.placedPieces.filter(_._2.team != team)
      val noEnemyCanAttackTheKing = enemies.map{ case (coordinate, enemy) =>
        MovementRules().rules(MovePiece(board, coordinate, kingPosition, enemy))
      }.collectFirst{
        case Right(enemyCanAttackTheKing) => enemyCanAttackTheKing
      }.isEmpty
      noEnemyCanAttackTheKing
  }

}
case class CheckRules()(implicit untouchedPieces: Set[Coordinate])  {


  case object `Cannot move to this square: The king would be in check` extends MoveRejection {
    def isValid = {
      case movePiece@MovePiece(board, from, to, PlacedPiece.PlacedKing(team)) =>
        CheckRules `the King is in check` (
          board = movePiece.board apply movePiece,
          team = team
        )
      case _ => true
    }
  }


}