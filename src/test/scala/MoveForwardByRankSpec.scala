import domain.model.chessboard.{Chessboard, Coordinate}
import domain.model.chesspieces.Chesspiece.{Team, White}
import domain.model.chesspieces.{Horse, Pawn}
import domain.services.MoveForwardByRank
import org.scalatest.flatspec.AnyFlatSpec

class MoveForwardByRankSpec extends AnyFlatSpec {

  "Applying the move forward by rank" should " move pawn first to the frontline, leaving the other pieces untouched." in {
    implicit val team: Team = White
    val pawn = Pawn(Coordinate(0,2), "Pawn1", team)
    val horse = Horse(Coordinate(1,2), "Horse1", team)
    val chessboard = Chessboard(Set(pawn, horse))
    val strategy: MoveForwardByRank = new MoveForwardByRank()
    val next: Chessboard = strategy.move(chessboard)
    assert(next.chesspieces.toSeq(0).coordinate == Coordinate(0,1))
    assert(next.chesspieces.toSeq(1).coordinate == horse.coordinate)
  }

}
