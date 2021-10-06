package domain.model.chesspieces


sealed trait PlacedPiece extends Team.HasTeam with Moves.HasMoves {
  val piece: Piece
}
object PlacedPiece {
  case class PlacedPawn(team: Team) extends PlacedPiece {
    val piece = Piece.Pawn
    import Moves._
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
    import Moves._
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
    val piece = Piece.Tower
    import PlacedTower._
    override val moves = towerMoves
  }
  object PlacedTower {
    import Moves._
    val towerMoves: Set[DomanSpecificLanguage] = Set(
      Line(Forward),
      Line(Backwards),
      Line(Right),
      Line(Left)
    )
  }
  case class PlacedRook(team: Team) extends PlacedPiece {
    val piece = Piece.Rook
    import PlacedRook._
    override val moves = rookMoves
  }
  object PlacedRook {
    import Moves._
    val rookMoves: Set[DomanSpecificLanguage] = Set(
      Line(diagonals.ForwardRight),
      Line(diagonals.ForwardLeft),
      Line(diagonals.BackwardsRight),
      Line(diagonals.BackwardsLeft)
    )
  }
  case class PlacedQueen(team: Team) extends PlacedPiece {
    val piece = Piece.Queen
    val moves = PlacedTower.towerMoves ++ PlacedRook.rookMoves
  }
  case class PlacedKing(team: Team) extends PlacedPiece {
    val piece = Piece.King
    val moves = PlacedKing.moves
  }
  object PlacedKing {
    import Moves._
    val moves: Set[DomanSpecificLanguage] = Set(
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