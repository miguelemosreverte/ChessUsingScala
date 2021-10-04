package domain.model.chesspieces

import domain.model.chessboard.Coordinate.Moves
import domain.model.chessboard.Coordinate

case class Pawn(coordinate: Coordinate, id: String, givenTeam: Chesspiece.Team) extends Chesspiece {
  val rank = 0
  override implicit val team: Chesspiece.Team = givenTeam
  override val availableMoves: Seq[Coordinate] = Seq(
    coordinate.forward,
  )
  override def move(coordinate: Coordinate): Chesspiece = copy(
    coordinate = coordinate
  )
}
