package domain.services.rules

import domain.model.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}
import BasicRules._
import ChessRules.Move
import UserMovementRules.placePiece
import domain.model.{Board, Coordinate, Team}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasicRulesSpec extends AnyWordSpec with Matchers {

  "piece moving to the same square it was in" should {
    "fail" in {
      object scenario {
        val piece = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 1)
        val board = Board(
          placedPieces = Map(from -> piece)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Piece cannot move to same coordinate it is located`)
      )
    }
  }

  "piece cannot attack allies" should {
    "fail" in {
      object scenario {
        val tower1 = PlacedTower(Team.White)
        val tower2 = PlacedTower(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
        val board = Board(
          placedPieces = Map(from -> tower1, to -> tower2)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Piece cannot replace piece from same team`)
      )
    }
  }

}
