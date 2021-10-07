import domain.model.Coordinate

object UserInput {
  case object ParseFailure
  val letterToNumber = ('a' to 'h').map(_.toString).zipWithIndex.map { case (l, i) => l -> (i + 1) }.toMap

  def parseToCoordinatePair = parse.andThen(toCoordinatePair)

  def parse: String => Array[Int] = { line =>
    line.toList.map(_.toString) match {
      case x1 :: y1 :: x2 :: y2 :: Nil
          if letterToNumber.isDefinedAt(x1)
          && letterToNumber.isDefinedAt(x2) =>
        Array(letterToNumber(x1), y1.toInt, letterToNumber(x2), y2.toInt)
      case _ => Array()
    }
  }
  def toCoordinatePair(line: Array[Int]): Either[ParseFailure.type, (Coordinate, Coordinate)] =
    line.toList match {
      case x1 :: y1 :: x2 :: y2 :: Nil =>
        Right((Coordinate(x1, y1), Coordinate(x2, y2)))
      case _ => Left(ParseFailure)
    }

}
