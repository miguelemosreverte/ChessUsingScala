package domain.model.chesspieces.rules
import domain.model.chesspieces.rules.ChessRules._
import domain.model.chesspieces._

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

case class MovementRules()(implicit untouchedPieces: Set[Coordinate])
    extends ChessRules {
  def rules(implicit move: MovePiece): Either[ChessRules.MoveRejection, Board] =
    for {
      _ <- `Piece cannot move like this`.validate
    } yield move.board

  case object `Piece cannot move like this` extends MoveRejection {
    def isValid = {
      case MovePiece(board, from, to, piece) => {
        val availableMoves: Set[Coordinate] =
          piece.moves.flatMap(e => Maria.interpretDetailed(from, e)(board, untouchedPieces, piece.team))
        availableMoves contains to
      }
    }
  }

}