import domain.model.{Board, Coordinate}
import domain.services.rules.ChessRules.{Move, MovePiece, MoveRejection}
import domain.services.rules.UserMovementRules
import scala.io.StdIn.readLine

object Replay extends App {

  new UserInputFileAdapter("./resources/data/sample-moves.txt")
    .getCoordinatesPairs()
    .foldLeft[Either[MoveRejection, Board]](Right(Board.initial)) {
      case (Right(board), (from, to)) =>
        UserMovementRules.placePiece(Move(board, from, to))
      case (error, _) => error
    } match {
    case Left(value) => println("Could not replay game")
    case Right(board) => println(BoardRender.render(board))

  }

}
