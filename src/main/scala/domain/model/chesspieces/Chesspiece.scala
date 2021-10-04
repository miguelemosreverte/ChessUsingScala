package domain.model.chesspieces

import domain.model.chessboard.Coordinate

trait Chesspiece {
  val id: String
  implicit val team: Chesspiece.Team
  val coordinate: Coordinate
  val availableMoves: Seq[Coordinate]
  val rank: Int

  def move(coordinate: Coordinate): Chesspiece

  override def equals(o: Any): Boolean =
    o match {
      case c: Chesspiece if c.team == team && c.id == id => true
      case _ => false
    }

  
}

object Chesspiece {
  sealed trait Team {
    def forward: Int
  }
  case object White extends Team { def forward: Int = 1 }
  case object Black extends Team { def forward: Int = -1 }

}