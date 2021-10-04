package domain.services

import domain.model.chessboard.Chessboard

trait Move {
  def move(chessboard: Chessboard): Chessboard
}
