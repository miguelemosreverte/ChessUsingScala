package domain.services.movement.interpreter

import domain.model.{Coordinate, Moves, Team}

object MoveInterpreter {

  import domain.model.Moves._
  def interpretMove(originalCoordinate: Coordinate, move: Moves)(implicit team: Team): Coordinate = {
    val perspective = team match {
      case Team.White => (-1, 1)
      case Team.Black => (1, -1)
    }
    val (backwards, forward) = perspective
    val (left, right) = perspective
    move match {
      case Moves.Forward =>
        originalCoordinate.copy(
          y = originalCoordinate.y + forward
        )
      case Moves.Backwards =>
        originalCoordinate.copy(
          y = originalCoordinate.y + backwards
        )
      case Moves.Left =>
        originalCoordinate.copy(
          x = originalCoordinate.x + left
        )
      case Moves.Right =>
        originalCoordinate.copy(
          x = originalCoordinate.x + right
        )
      case diagonals.ForwardLeft =>
        originalCoordinate.copy(
          x = originalCoordinate.x + left,
          y = originalCoordinate.y + forward
        )
      case diagonals.ForwardRight =>
        originalCoordinate.copy(
          x = originalCoordinate.x + right,
          y = originalCoordinate.y + forward
        )
      case diagonals.BackwardsLeft =>
        originalCoordinate.copy(
          x = originalCoordinate.x + left,
          y = originalCoordinate.y + backwards
        )
      case diagonals.BackwardsRight =>
        originalCoordinate.copy(
          x = originalCoordinate.x + right,
          y = originalCoordinate.y + backwards
        )
    }
  }

}
