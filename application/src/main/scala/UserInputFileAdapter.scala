import com.whitehatgaming.UserInputFile
import domain.model.{Board, Coordinate}

class UserInputFileAdapter(fileName: String) extends UserInputFile(fileName) {
  import UserInput._

  val lines = scala.io.Source.fromFile(fileName).getLines
  val linesIterator = lines.iterator

  def getCoordinatesPairs(moves: Seq[(Coordinate, Coordinate)] = Seq.empty): Seq[(Coordinate, Coordinate)] =
    nextCoordinatePair() match {
      case Some((from, to)) =>
        getCoordinatesPairs(moves :+ (from, to))
      case None => moves
    }

  def nextCoordinatePair(): Option[(Coordinate, Coordinate)] =
    toCoordinatePair(nextMove()).toOption

  override def nextMove(): Array[Int] = {
    if (linesIterator.iterator.isEmpty) Array()
    else
      parse(linesIterator.next())
  }

}
