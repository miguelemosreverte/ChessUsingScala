import domain.model.{Board, Coordinate}
import domain.services.rules.ChessRules.{Move, MovePiece, MoveRejection}
import domain.services.rules.UserMovementRules
import scala.io.StdIn.readLine

object NewGame extends App {

  println("""
      |
      |Hello!
      |This is a chess game.
      |To move a pawn, write e4e6
      |""".stripMargin)

  game(Board.initial)

  def game(board: Board): Unit = {
    UserInput
      .parseToCoordinatePair(readLine())
      .fold(
        { error =>
          println("""
              |Error in input: 
              |Remember to write chess commands like: 
              |a1a2 for pawns 
              |or 
              |b3b5 for towers.
              |
              |""".stripMargin)
          game(board)
        }, {
          case (from, to) =>
            UserMovementRules
              .placePiece(Move(board, from, to))
              .fold(
                { moveRejection =>
                  println(s"Move rejection: $moveRejection")
                  game(board)
                }, { board =>
                  println(BoardRender.render(board))
                  game(board)
                }
              )

        }
      )
  }

}
