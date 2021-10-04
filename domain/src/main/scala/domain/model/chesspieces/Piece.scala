package domain.model.chesspieces

import domain.model.chesspieces.ChessRules.{Move, MoveRejection}
import domain.model.chesspieces.PlacedPiece.{PlacedKing, PlacedPawn, PlacedTower}

sealed trait Piece
object Piece {
  case object Pawn extends Piece
  case object Horse extends Piece
  case object Rook extends Piece
  case object Tower extends Piece
  case object Queen extends Piece
  case object King extends Piece
}

sealed trait Team
object Team {
  case object White extends Team
  case object Black extends Team

  trait HasTeam { val team: Team }
}

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
case class Coordinate(x: Int, y: Int)
case class Board(placedPieces: Map[Coordinate, PlacedPiece])

object ChessRules {
  case class Move(board: Board, from: Coordinate, to: Coordinate)

  trait MoveRejection {
    protected def isValid: Move => Boolean
    final def validate()(implicit move: Move): Either[MoveRejection, Unit] =
      isValid(move) match {
        case true => Right(())
        case false => Left(this)
      }
  }
}
trait ChessRules {
  def rules(implicit move: ChessRules.Move): Either[ChessRules.MoveRejection, Board]
}

object BoardBasics extends ChessRules {
  def rules(implicit move: Move): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece is not in board`.validate
    } yield move.board

  case object `Piece is not in board` extends MoveRejection {
    def isValid = {
      case Move(board, from, to) =>
        board.placedPieces contains from
    }
  }
}
case class BasicRules()(implicit team: Team) extends ChessRules {
  def rules(implicit move: Move): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece cannot move to same coordinate it is located`.validate
      _ <- `Piece cannot replace piece from same team`.validate
    } yield move.board

  case object `Piece cannot move to same coordinate it is located` extends MoveRejection {
    def isValid = {
      case Move(board, from, to) => from != to
    }
  }

  case object `Piece cannot replace piece from same team` extends MoveRejection {
    def isValid = {
      case move @ Move(board, from, to) =>
        board.placedPieces.get(to) match {
          case Some(to) => team != to.team
          case None => true
        }
    }
  }

}

object Maria {
  import Moves._
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

  def interpretDetailed(
      originalCoordinate: Coordinate,
      detailed: DomanSpecificLanguage
  )(implicit board: Board, untouchedPieces: Set[Coordinate], team: Team): Seq[Coordinate] = {
    (detailed match {
      case Moves.Single(move) =>
        Seq(interpretMove(originalCoordinate, move))
      case Moves.Jump(across) =>
        Seq(across.foldLeft(originalCoordinate)((nextCoordinate, move) => interpretMove(nextCoordinate, move)))
      case Moves.Line(direction) =>
        val path = (1 to 12)
          .foldLeft(Seq(originalCoordinate))((nextCoordinate, move) =>
            nextCoordinate :+ interpretMove(nextCoordinate.last, direction)
          )
          .tail
        path.find(c => board.placedPieces.contains(c)) match {
          case Some(value) => path.takeWhile(c => !board.placedPieces.contains(c)) :+ value
          case None => path
        }
      case withPredicate(move, predicate) =>
        val path = interpretDetailed(originalCoordinate, move)
        if (predicate(
              Context(
                `there is a piece to attack` = path.flatMap(board.placedPieces.get).exists(_.team != team),
                `there is nothing in the way` = path.flatMap(board.placedPieces.get).isEmpty,
                `untouched from the start of the game` = untouchedPieces contains originalCoordinate
              )
            ))
          path
        else Seq()

    }).filter {
      case Coordinate(x, y) if x < 0 || y < 0 || x > 12 || y > 12 => false
      case validCoordinate => true
    }
  }

}

case class MovementRules()(implicit availableMovements: Set[Moves.DomanSpecificLanguage],
                           board: Board,
                           team: Team,
                           untouchedPieces: Set[Coordinate])
    extends ChessRules {
  def rules(implicit move: Move): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece cannot move like this`.validate
    } yield move.board

  case object `Piece cannot move like this` extends MoveRejection {
    def isValid = {
      case Move(board, from, to) => {
        val availableMoves: Set[Coordinate] =
          availableMovements.flatMap(e => Maria.interpretDetailed(from, e)(board, untouchedPieces, team))
        availableMoves contains to
      }
    }
  }

}
object RuleEngine {

  def placePiece(move: Move)(implicit untouchedPieces: Set[Coordinate]) = {
    implicit val mo = move
    move.board.placedPieces.get(move.from) match {
      case None => Left(BoardBasics.`Piece is not in board`)
      case Some(from) =>
        implicit val team: Team = from.team
        implicit val availableMovements = from.moves
        implicit val board = move.board
        val basicRules = BasicRules.apply
        import basicRules._
        val movementRules = MovementRules.apply()
        import movementRules._
        for {
          _ <- `Piece cannot move to same coordinate it is located`.validate
          _ <- `Piece cannot move like this`.validate
          done <- `Piece cannot replace piece from same team`.validate
        } yield Board(
          placedPieces = {
            val without: Map[Coordinate, PlacedPiece] = (move.board.placedPieces - move.from - move.to)
            val withIt: Map[Coordinate, PlacedPiece] = without.+((move.to, from))
            withIt
          }
        )
    }

  }

}
object Pepe extends App {

  object dataset {

    implicit val untouchedPieces = Set(
      Coordinate(1, 1)
    )
    val empty = Board(
      Map.empty
    )

    val singlePawn = Board(
      Map(
        Coordinate(1, 1) -> PlacedPawn(Team.White)
      )
    )

    val twoEnemyTowers = Board(
      Map(
        Coordinate(1, 1) -> PlacedTower(Team.White),
        Coordinate(1, 2) -> PlacedTower(Team.Black)
      )
    )

    val twoFriendlyTowers = Board(
      Map(
        Coordinate(1, 1) -> PlacedTower(Team.White),
        Coordinate(1, 2) -> PlacedTower(Team.White)
      )
    )

    val twoOppositePawns = Board(
      Map(
        Coordinate(1, 1) -> PlacedPawn(Team.White),
        Coordinate(1, 2) -> PlacedPawn(Team.Black)
      )
    )

    val twoNearPawns = Board(
      Map(
        Coordinate(1, 1) -> PlacedPawn(Team.White),
        Coordinate(2, 2) -> PlacedPawn(Team.Black)
      )
    )
  }

  import RuleEngine._
  import dataset.untouchedPieces
  {
    import dataset.empty
    // piece should not move to the same square it was located in
    println(placePiece(Move(empty, Coordinate(0, 0), Coordinate(1, 1)))) // Left(Piece is not in board)
  }

  {
    import dataset.singlePawn
    // piece should not move to the same square it was located in
    println(placePiece(Move(singlePawn, Coordinate(1, 1), Coordinate(1, 1)))) // Left(Piece cannot move to same coordinate it is located)

    // pawn should not move more than one square
    println(placePiece(Move(singlePawn, Coordinate(1, 1), Coordinate(1, 6)))) // Left(Piece cannot move like this)

    // pawn should can move more than one square if its been untouched since beggining of the game
    println(placePiece(Move(singlePawn, Coordinate(1, 1), Coordinate(1, 3)))(Set(Coordinate(1, 1)))) // Right(Board(Map(Coordinate(1,3) -> PlacedPawn(White))))

    // other pawn should cannot move more than one square if its been touched since beggining of the game
    println(placePiece(Move(singlePawn, Coordinate(1, 1), Coordinate(1, 3)))(Set.empty)) // Left(Piece cannot move like this)

  }

  {
    import dataset.twoFriendlyTowers
    // tower should not eat friendly pawn
    println(placePiece(Move(twoFriendlyTowers, Coordinate(1, 1), Coordinate(1, 2)))) // Left(Piece cannot replace piece from same team)
  }

  {
    import dataset.twoEnemyTowers
    // tower should eat tower in front
    println(placePiece(Move(twoEnemyTowers, Coordinate(1, 1), Coordinate(1, 2)))) // Right(Board(Map(Coordinate(1,2) -> PlacedPawn(White))))
    println(placePiece(Move(twoEnemyTowers, Coordinate(1, 2), Coordinate(1, 1)))) // Right(Board(Map(Coordinate(1,2) -> PlacedPawn(Black))))

    // tower should move across a line
    println(placePiece(Move(twoEnemyTowers, Coordinate(1, 2), Coordinate(1, 5)))) // Right(Board(Map(Coordinate(1,1) -> PlacedPawn(White), Coordinate(1,5) -> PlacedTower(Black))))

    // but first tower cannot move across a line because is blocked by the other tower
    println(placePiece(Move(twoEnemyTowers, Coordinate(1, 1), Coordinate(1, 5)))) // Left(Piece cannot move like this)

  }

  {
    import dataset.twoOppositePawns
    // pawn should block the other pawn advance
    println(placePiece(Move(twoOppositePawns, Coordinate(1, 1), Coordinate(1, 2)))) // Left(Piece cannot move like this)
    println(placePiece(Move(twoOppositePawns, Coordinate(1, 2), Coordinate(1, 1)))) // Left(Piece cannot move like this)
  }

  {
    import dataset.twoNearPawns
    // pawn should attack the other pawn
    println(placePiece(Move(twoNearPawns, Coordinate(1, 1), Coordinate(2, 2)))) // Right(Board(Map(Coordinate(2,2) -> PlacedPawn(White))))
    println(placePiece(Move(twoNearPawns, Coordinate(2, 2), Coordinate(1, 1)))) // Right(Board(Map(Coordinate(2,2) -> PlacedPawn(White))))
  }

}
