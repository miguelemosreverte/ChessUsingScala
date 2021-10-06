package domain.model.chesspieces

import domain.model.chesspieces.rules.ChessRules.MovePiece

import scala.collection.immutable

case class Board(placedPieces: Map[Coordinate, PlacedPiece]) {
  def apply(movePiece: MovePiece): Board =
    Board(
      placedPieces - movePiece.from - movePiece.to + (movePiece.to -> movePiece.piece)
    )
}

object Board extends App {
  val initial = Board(
    Map(
      Coordinate(1,1) -> PlacedPiece.PlacedPawn(Team.White)
    )
  )

  println("Rendering")
  println(render(initial))


  // todo move elsewhere

  sealed trait Square
  case object Empty  extends Square
  case class Used(piece: PlacedPiece) extends Square


  implicit class RenderablePiece(piece: PlacedPiece) {
    def render: String = piece.piece match {
      case Piece.Pawn => s"P1w"
      case Piece.Horse => s"H"
      case Piece.Rook => s"R"
      case Piece.Tower => s"T"
      case Piece.Queen => s"Q"
      case Piece.King => s"K"
    }
  }
  implicit class RenderableSquare(square: Square) {
    def render: String = square match {
      case Empty =>
        s"""
           ||--------|
           ||        |
           ||        |
           ||        |
           ||--------|""".stripMargin
      case Used(piece) =>
        val `piece renders to two characters` = piece.render
        val ie = `piece renders to two characters`
        s"""
           ||--------|
           ||        |
           ||  $ie   |
           ||        |
           ||--------|""".stripMargin
    }
  }
  implicit class RenderableBoard(board: Board) {
    def get(coordinate: Coordinate): Square = {
      board.placedPieces.get(coordinate) match {
        case Some(value) => Used(value)
        case None =>Empty
      }
    }
  }



  def render(board: Board): String = {
    val minIndex = 0
    val maxIndex = 7
    val proposedSquareRenders = (minIndex to maxIndex).map { x =>
      (minIndex to maxIndex).map { y =>
        board.get(Coordinate(x, y)).render
      }
    }



    val rendered = proposedSquareRenders.map { row =>
      val naive: Seq[String] = row.flatMap {square =>
        square.split("\n").zipWithIndex
      }.groupBy(_._2).toSeq.sortBy(_._1).map { case (index, lineImages) =>
        lineImages.map(_._1).mkString("")
      }

      //val withLastBorderBottom = naive ++ Seq(naive.head)

      naive.mkString("\n")

    }

    println(rendered.mkString)
    ""
  }
}