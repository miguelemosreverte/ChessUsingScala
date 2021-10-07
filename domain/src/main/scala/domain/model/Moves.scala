package domain.model

sealed trait Moves
object Moves {
  case object Forward extends Moves

  case object Backwards extends Moves

  case object Left extends Moves

  case object Right extends Moves

  object diagonals {
    case object ForwardLeft extends Moves
    case object ForwardRight extends Moves
    case object BackwardsLeft extends Moves
    case object BackwardsRight extends Moves
  }
}
