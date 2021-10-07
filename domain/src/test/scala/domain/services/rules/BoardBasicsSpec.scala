package domain.services.rules

import domain.model.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}
import BasicRules._
import BoardBasics._
import CheckRules._
import ChessRules.Move
import UserMovementRules.placePiece
import domain.model.{Board, Coordinate}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardBasicsSpec extends AnyWordSpec with Matchers {

  "moving a piece that is not in board" should {
    "fail" in {
      object scenario {
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
        val board = Board(
          placedPieces = Map.empty
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(Left(`Piece is not in board`))
    }
  }

}
