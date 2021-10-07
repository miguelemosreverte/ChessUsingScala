package domain.services.rules

import domain.model.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}
import ChessRules.Move
import UserMovementRules.placePiece
import domain.model.{Board, Coordinate, Team}
import domain.services.rules.MovementRules._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MovementRulesSpec extends AnyWordSpec with Matchers {

  "pawn one square" should {
    "succeed" in {
      object scenario {
        val piece = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
        val board = Board(
          placedPieces = Map(from -> piece)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Right(Board(Map(Coordinate(1, 2) -> PlacedPawn(Team.White)), Set()))
      )
    }
  }

  "pawn advancing more than one square" should {
    "fail" in {
      object scenario {
        val piece = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 4)
        val board = Board(
          placedPieces = Map(from -> piece)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(Left(`Piece cannot move like this`))
    }
  }

  "pawn advancing two squares if it was left untouched since start of game" should {
    "succeed" in {
      object scenario {
        val piece = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 3)
        val board = Board(
          placedPieces = Map(from -> piece),
          untouchedPieces = Set(from)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Right(
          Board(
            placedPieces = Map(to -> piece),
            untouchedPieces = Set.empty
          )
        )
      )
    }
    "fail" in {
      object scenario {
        val piece = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 3)
        val board = Board(
          placedPieces = Map(from -> piece),
          untouchedPieces = Set.empty
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Piece cannot move like this`)
      )
    }
  }

  "piece cannot move across other pieces" should {
    "fail" in {
      object scenario {
        val tower1 = PlacedTower(Team.White)
        val tower2 = PlacedTower(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
        val invalidMove = Coordinate(1, 3)
        val board = Board(
          placedPieces = Map(from -> tower1, to -> tower2)
        )
      }
      import scenario._
      placePiece(Move(board, from, invalidMove)) must be(
        Left(`Piece cannot move like this`)
      )
    }
  }

  "pawn should not advance if other piece is in front" should {
    "fail" in {
      object scenario {
        val whitePawn = PlacedPawn(Team.White)
        val blackPawn = PlacedPawn(Team.Black)
        val from = Coordinate(1, 1)
        val to = Coordinate(2, 1)
        val board = Board(
          placedPieces = Map(from -> whitePawn, to -> blackPawn)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Piece cannot move like this`)
      )
    }
  }

  "pawn should attack diagonally if there is enemy to attack" should {
    "succeed" in {
      object scenario {
        val whitePawn = PlacedPawn(Team.White)
        val blackPawn = PlacedPawn(Team.Black)
        val from = Coordinate(1, 1)
        val to = Coordinate(2, 2)
        val board = Board(
          placedPieces = Map(from -> whitePawn, to -> blackPawn),
          untouchedPieces = Set.empty
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Right(board.copy(placedPieces = Map(to -> whitePawn)))
      )
    }
    "fail" in {
      object scenario {
        val whitePawn = PlacedPawn(Team.White)
        val from = Coordinate(1, 1)
        val to = Coordinate(2, 2)
        val board = Board(
          placedPieces = Map(from -> whitePawn)
        )
      }
      import scenario._
      placePiece(Move(board, from, to)) must be(
        Left(`Piece cannot move like this`)
      )
    }
  }

}
