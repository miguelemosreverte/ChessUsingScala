package domain.services.movement.algebra

import domain.model.Moves

sealed trait ChessLanguage
object ChessLanguage {
  case class Line(direction: Moves) extends ChessLanguage

  case class Jump(across: Seq[Moves]) extends ChessLanguage

  case class Single(to: Moves) extends ChessLanguage

  case class Context(`there is a piece to attack`: Boolean,
                     `there is nothing in the way`: Boolean,
                     `untouched from the start of the game`: Boolean)

  case class withPredicate(move: ChessLanguage, predicate: Context => Boolean) extends ChessLanguage

}
