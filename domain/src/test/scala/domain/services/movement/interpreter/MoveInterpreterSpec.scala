package domain.services.movement.interpreter

import domain.model.PlacedPiece.{PlacedPawn, PlacedTower}
import domain.model.{Board, Coordinate, Moves, Team}
import domain.services.rules.ChessRules.Move
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MoveInterpreterSpec extends AnyWordSpec with Matchers {

  "piece moving" should {
    "increase coordinate if its from Team.White" in {
      object scenario {
        implicit val team: Team = Team.White
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 2)
      }
      import scenario._
      MoveInterpreter.interpretMove(from, Moves.Forward) must be(to)
    }
    "decrease coordinate if its from Team.Black" in {
      object scenario {
        implicit val team: Team = Team.Black
        val from = Coordinate(1, 1)
        val to = Coordinate(1, 0)
      }
      import scenario._
      MoveInterpreter.interpretMove(from, Moves.Forward) must be(to)
    }
  }

}
