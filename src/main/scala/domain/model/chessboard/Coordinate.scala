package domain.model.chessboard

import domain.model.chessboard.Coordinate
import domain.model.chesspieces.Chesspiece.Team

case class Coordinate(x: Int, y: Int)

object Coordinate {
  implicit class Moves(coordinate: Coordinate) {
    def backward(implicit team: Team) = coordinate.copy( y = -team.forward)
    def forward(implicit team: Team) = coordinate.copy( y = team.forward)
    def left = coordinate.copy( x = coordinate.x - 1)
    def right = coordinate.copy( x = coordinate.x + 1)
  }
}
