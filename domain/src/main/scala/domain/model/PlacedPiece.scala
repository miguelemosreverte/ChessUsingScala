package domain.model

import domain.model.Moves._
import domain.services.movement.algebra.ChessLanguage._
import domain.services.movement.algebra.ChessLanguage

sealed trait PlacedPiece {
  val team: Team
  val moves: Set[ChessLanguage]
  val piece: Piece
}
object PlacedPiece {
  case class PlacedPawn(team: Team) extends PlacedPiece {
    val piece = Piece.Pawn
    val moves = Set(
      withPredicate(Jump(Seq(Forward, Forward)), { context =>
        context.`untouched from the start of the game` &&
        context.`there is nothing in the way`
      }),
      withPredicate(Single(Forward), { _.`there is nothing in the way` }),
      withPredicate(Single(diagonals.ForwardLeft), { _.`there is a piece to attack` }),
      withPredicate(Single(diagonals.ForwardRight), { _.`there is a piece to attack` })
    )
  }

  case class PlacedHorse(team: Team) extends PlacedPiece {
    val piece = Piece.Horse
    val moves = Set(
      Jump(Seq(Forward, Forward, Right)),
      Jump(Seq(Forward, Forward, Left)),
      Jump(Seq(Backwards, Backwards, Right)),
      Jump(Seq(Backwards, Backwards, Right)),
      Jump(Seq(Right, Right, Forward)),
      Jump(Seq(Right, Right, Backwards)),
      Jump(Seq(Left, Left, Forward)),
      Jump(Seq(Left, Left, Backwards))
    )
  }
  case class PlacedTower(team: Team) extends PlacedPiece {
    override val moves = PlacedTower.towerMoves
    import PlacedTower._
    val piece = Piece.Tower
  }

  case class PlacedRook(team: Team) extends PlacedPiece {
    override val moves = PlacedRook.rookMoves
    import PlacedRook._
    val piece = Piece.Rook
  }

  case class PlacedQueen(team: Team) extends PlacedPiece {
    val piece = Piece.Queen
    val moves = PlacedTower.towerMoves ++ PlacedRook.rookMoves
  }

  case class PlacedKing(team: Team) extends PlacedPiece {
    val piece = Piece.King
    val moves = PlacedKing.moves
  }

  object PlacedTower {
    val towerMoves: Set[ChessLanguage] = Set(
      Line(Forward),
      Line(Backwards),
      Line(Right),
      Line(Left)
    )
  }

  object PlacedRook {
    val rookMoves: Set[ChessLanguage] = Set(
      Line(diagonals.ForwardRight),
      Line(diagonals.ForwardLeft),
      Line(diagonals.BackwardsRight),
      Line(diagonals.BackwardsLeft)
    )
  }

  object PlacedKing {
    val moves: Set[ChessLanguage] = Set(
      Single(Forward),
      Single(Backwards),
      Single(Right),
      Single(Left),
      Single(diagonals.ForwardRight),
      Single(diagonals.ForwardLeft),
      Single(diagonals.BackwardsRight),
      Single(diagonals.BackwardsLeft),
      withPredicate(Jump(Seq(Left, Left, Left)), { c =>
        c.`untouched from the start of the game` && c.`there is nothing in the way`
      })
    )
  }
}
