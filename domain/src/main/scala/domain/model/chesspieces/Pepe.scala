package domain.model.chesspieces

import domain.model.chesspieces.PlacedPiece._
import domain.model.chesspieces.rules.ChessRules.Move
import domain.model.chesspieces.rules.UserMovementRules.placePiece
import com.whitehatgaming.UserInputFile
import scala.io.StdIn.readLine
object Maria extends App {
  class UserInputFileAdapter(fileName: String) extends UserInputFile(fileName) {
    val lines = scala.io.Source.fromFile(fileName).getLines
    val linesIterator = lines.iterator
    val letterToNumber = ('a' to 'h').map(_.toString).zipWithIndex.map { case (l, i) => l -> (i + 1) }.toMap
    override def nextMove(): Array[Int] =
      linesIterator.next().toList.map(_.toString) match {
        case x1 :: y1 :: x2 :: y2 :: Nil
          if letterToNumber.isDefinedAt(x1)
            && letterToNumber.isDefinedAt(x2) =>
          Array(letterToNumber(x1), y1.toInt, letterToNumber(x2), y2.toInt)
        case _ => Array()
      }

    def nextCoordinatePair(): Option[(Coordinate, Coordinate)] =
      nextMove().toList match {
        case x1 :: y1 :: x2 :: y2 :: Nil  =>
          Some((Coordinate(x1, y1), Coordinate(x2, y2)))
        case _ => None
      }

  }
  val adapter = new UserInputFileAdapter("./resources/data/checkmate.txt")


  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())
  println(adapter.nextCoordinatePair())

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
