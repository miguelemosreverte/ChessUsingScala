package domain.model

import domain.services.rules.ChessRules.MovePiece

case class Board(placedPieces: Map[Coordinate, PlacedPiece], untouchedPieces: Set[Coordinate]) {
  def apply(movePiece: MovePiece): Board = {
    println(untouchedPieces)
    Board(
      placedPieces = placedPieces - movePiece.from - movePiece.to + (movePiece.to -> movePiece.piece),
      untouchedPieces = untouchedPieces - movePiece.from
    )
  }
}
object Board {
  def apply(placedPieces: Map[Coordinate, PlacedPiece]): Board =
    Board(
      placedPieces,
      untouchedPieces = placedPieces.keys.toSet
    )

  val initial = Board.apply({
    val whiteTeam = Map(
      Coordinate(1, 1) -> PlacedPiece.PlacedTower(Team.White),
      Coordinate(2, 1) -> PlacedPiece.PlacedHorse(Team.White),
      Coordinate(3, 1) -> PlacedPiece.PlacedRook(Team.White),
      Coordinate(4, 1) -> PlacedPiece.PlacedKing(Team.White),
      Coordinate(5, 1) -> PlacedPiece.PlacedQueen(Team.White),
      Coordinate(6, 1) -> PlacedPiece.PlacedRook(Team.White),
      Coordinate(7, 1) -> PlacedPiece.PlacedHorse(Team.White),
      Coordinate(8, 1) -> PlacedPiece.PlacedTower(Team.White),
      Coordinate(1, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(2, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(3, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(4, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(5, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(6, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(7, 2) -> PlacedPiece.PlacedPawn(Team.White),
      Coordinate(8, 2) -> PlacedPiece.PlacedPawn(Team.White)
    )

    val blackTeam = Map(
      Coordinate(1, 8) -> PlacedPiece.PlacedTower(Team.Black),
      Coordinate(2, 8) -> PlacedPiece.PlacedHorse(Team.Black),
      Coordinate(3, 8) -> PlacedPiece.PlacedRook(Team.Black),
      Coordinate(4, 8) -> PlacedPiece.PlacedQueen(Team.Black),
      Coordinate(5, 8) -> PlacedPiece.PlacedKing(Team.Black),
      Coordinate(6, 8) -> PlacedPiece.PlacedRook(Team.Black),
      Coordinate(7, 8) -> PlacedPiece.PlacedHorse(Team.Black),
      Coordinate(8, 8) -> PlacedPiece.PlacedTower(Team.Black),
      Coordinate(1, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(2, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(3, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(4, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(5, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(6, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(7, 7) -> PlacedPiece.PlacedPawn(Team.Black),
      Coordinate(8, 7) -> PlacedPiece.PlacedPawn(Team.Black)
    )

    whiteTeam ++ blackTeam
  })
}
