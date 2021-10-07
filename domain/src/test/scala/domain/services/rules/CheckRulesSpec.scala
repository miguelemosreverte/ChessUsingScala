package domain.services.rules

import domain.model.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}
import CheckRules._
import ChessRules.Move
import UserMovementRules.placePiece
import domain.model.{Board, Coordinate, Team}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CheckRulesSpec extends AnyWordSpec with Matchers {

  "cannot move if king is in check" should {
    "fail" in {
      object scenario {
        val whitePawn = PlacedPawn(Team.White)
        val whiteKing = PlacedKing(Team.White)
        val blackTower = PlacedTower(Team.Black)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
        val board = Board(
          placedPieces = Map(
            from -> whitePawn,
            Coordinate(3, 3) -> whiteKing,
            Coordinate(3, 4) -> blackTower
          )
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Cannot move to this square: The king would be in check`)
      )
    }
  }

}
