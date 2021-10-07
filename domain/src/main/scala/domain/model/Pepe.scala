package domain.model

import com.whitehatgaming.UserInputFile
import PlacedPiece._
import domain.services.rules.ChessRules.Move
import domain.services.rules.UserMovementRules.placePiece
object Maria extends App {
  val adapter = new UserInputFileAdapter("./resources/data/checkmate.txt")

  class UserInputFileAdapter(fileName: String) extends UserInputFile(fileName) {
    val lines = scala.io.Source.fromFile(fileName).getLines
    val linesIterator = lines.iterator
    val letterToNumber = ('a' to 'h').map(_.toString).zipWithIndex.map { case (l, i) => l -> (i + 1) }.toMap

    def nextCoordinatePair(): Option[(Coordinate, Coordinate)] =
      nextMove().toList match {
        case x1 :: y1 :: x2 :: y2 :: Nil =>
          Some((Coordinate(x1, y1), Coordinate(x2, y2)))
        case _ => None
      }

    override def nextMove(): Array[Int] =
      linesIterator.next().toList.map(_.toString) match {
        case x1 :: y1 :: x2 :: y2 :: Nil
            if letterToNumber.isDefinedAt(x1)
            && letterToNumber.isDefinedAt(x2) =>
          Array(letterToNumber(x1), y1.toInt, letterToNumber(x2), y2.toInt)
        case _ => Array()
      }

  }

  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())

}
