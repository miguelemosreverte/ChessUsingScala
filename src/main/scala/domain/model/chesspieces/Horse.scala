package domain.model.chesspieces

import domain.model.chessboard.Coordinate.Moves
import domain.model.chessboard.Coordinate

case class Horse(coordinate: Coordinate, id: String, givenTeam: Chesspiece.Team) extends Chesspiece {
  val rank = 1
  override implicit val team: Chesspiece.Team = givenTeam
  override val availableMoves: Seq[Coordinate] = Seq(
    coordinate.forward.forward.left,
    coordinate.forward.forward.right,
    coordinate.backward.backward.left,
    coordinate.backward.backward.right,
    coordinate.right.right.backward,
    coordinate.right.right.forward,
    coordinate.left.left.forward,
    coordinate.left.left.backward,
  )

  override def move(coordinate: Coordinate): Chesspiece = copy(
    coordinate = coordinate
  )
}
