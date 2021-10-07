package domain.services.movement.interpreter

import domain.model.{Board, Coordinate, Team}
import domain.services.movement.algebra.ChessLanguage

object ChessLanguage {
  import MoveInterpreter._
  import domain.services.movement.algebra.ChessLanguage._
  def interpret(
      originalCoordinate: Coordinate,
      description: ChessLanguage
  )(implicit board: Board, team: Team): Seq[Coordinate] = {
    (description match {
      case Single(move) =>
        Seq(interpretMove(originalCoordinate, move))
      case Jump(across) =>
        Seq(across.foldLeft(originalCoordinate)((nextCoordinate, move) => interpretMove(nextCoordinate, move)))
      case Line(direction) =>
        val path = (1 to 8)
          .foldLeft(Seq(originalCoordinate))((nextCoordinate, move) =>
            nextCoordinate :+ interpretMove(nextCoordinate.last, direction)
          )
          .tail
        path.find(c => board.placedPieces.contains(c)) match {
          case Some(value) => path.takeWhile(c => !board.placedPieces.contains(c)) :+ value
          case None => path
        }
      case withPredicate(move, predicate) =>
        val path = interpret(originalCoordinate, move)
        if (predicate(
              Context(
                `there is a piece to attack` = path.flatMap(board.placedPieces.get).exists(_.team != team),
                `there is nothing in the way` = path.flatMap(board.placedPieces.get).isEmpty,
                `untouched from the start of the game` = board.untouchedPieces contains originalCoordinate
              )
            ))
          path
        else Seq()

    }).filter {
      case Coordinate(x, y) if x < 1 || y < 1 || x > 8 || y > 8 => false
      case validCoordinate => true
    }
  }

}
