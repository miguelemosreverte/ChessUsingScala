package domain.services.rules

import domain.model.Piece.King
import ChessRules._
import domain.model.{Board, Team}

object CheckRules {

  def `the King is in check`(board: Board, team: Team) = {
    board.placedPieces
      .filter(_._2.piece == King)
      .filter(_._2.team == team)
      .toSeq
      .headOption match {
      case None => true
      case Some((kingPosition, king)) =>
        val enemies = board.placedPieces.filter(_._2.team != team)
        val noEnemyCanAttackTheKing = enemies
          .map {
            case (coordinate, enemy) =>
              MovementRules.rules(MovePiece(board, coordinate, kingPosition, enemy))
          }
          .collectFirst {
            case Right(enemyCanAttackTheKing) => enemyCanAttackTheKing
          }
          .isEmpty
        noEnemyCanAttackTheKing
    }

  }

  case object `Cannot move to this square: The king would be in check` extends MoveRejection {
    def isValid = {
      case movePiece @ MovePiece(board, from, to, piece) =>
        CheckRules `the King is in check` (
          board = movePiece.board apply movePiece,
          team = piece.team
        )
      case _ => true
    }
  }

}
