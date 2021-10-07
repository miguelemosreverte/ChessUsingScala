import domain.model.{Board, Coordinate, Piece, PlacedPiece, Team}

object BoardRender extends App {

  println("Rendering")
  println(render(Board.initial))

  // todo move elsewhere

  def render(board: Board): String = {
    val minIndex = 1
    val maxIndex = 8
    val proposedSquareRenders = (minIndex to maxIndex).map { x =>
      (minIndex to maxIndex).map { y =>
        board.get(Coordinate(x, y)).render
      }
    }

    val rendered = proposedSquareRenders.map { row =>
      val naive: Seq[String] = row
        .flatMap { square =>
          square.split("\n").zipWithIndex
        }
        .groupBy(_._2)
        .toSeq
        .sortBy(_._1)
        .map {
          case (index, lineImages) =>
            lineImages.map(_._1).mkString("")
        }

      //val withLastBorderBottom = naive ++ Seq(naive.head)

      naive.mkString("\n")

    }

    println(s"Rendering")
    rendered.mkString
  }

  sealed trait Square

  case class Used(piece: PlacedPiece) extends Square

  implicit class TeamRender(team: Team) {
    def render: String = team match {
      case Team.White => "white"
      case Team.Black => "black"
    }
  }

  implicit class PieceRender(piece: Piece) {
    def render: String = piece match {
      case Piece.Pawn => "pawn "
      case Piece.Horse => "horse"
      case Piece.Rook => "rook "
      case Piece.Tower => "tower"
      case Piece.Queen => "queen"
      case Piece.King => "king "
    }
  }

  implicit class RenderablePiece(piece: PlacedPiece) {
    def render: String = s"${piece.piece.render} ${piece.team.render}"
  }
  implicit class RenderableSquare(square: Square) {
    def render: String = square match {
      case Empty =>
        s"""
           ||--------------|
           ||              |
           ||              |
           ||              |
           ||              |
           ||              |
           ||--------------|""".stripMargin
      case Used(piece) =>
        val `piece renders to two characters` = piece.render
        val ie = `piece renders to two characters`
        s"""
           ||--------------|
           ||              |
           ||              |
           || $ie  |
           ||              |
           ||              |
           ||--------------|""".stripMargin
    }
  }
  implicit class RenderableBoard(board: Board) {
    def get(coordinate: Coordinate): Square = {
      board.placedPieces.get(coordinate) match {
        case Some(value) => Used(value)
        case None => Empty
      }
    }
  }

  case object Empty extends Square
}
