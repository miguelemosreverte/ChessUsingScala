package domain.model.chesspieces


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

  trait HasMoves { val moves: Set[DomanSpecificLanguage] }

  sealed trait DomanSpecificLanguage
  case class Line(direction: Moves) extends DomanSpecificLanguage
  case class Jump(across: Seq[Moves]) extends DomanSpecificLanguage
  case class Single(to: Moves) extends DomanSpecificLanguage

  case class Context(`there is a piece to attack`: Boolean,
                     `there is nothing in the way`: Boolean,
                     `untouched from the start of the game`: Boolean)
  case class withPredicate(move: DomanSpecificLanguage, predicate: Context => Boolean) extends DomanSpecificLanguage
}