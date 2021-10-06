package domain.model.chesspieces

sealed trait Team
object Team {
  case object White extends Team
  case object Black extends Team

  trait HasTeam { val team: Team }
}
