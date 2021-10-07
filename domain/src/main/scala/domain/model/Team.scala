package domain.model

sealed trait Team
object Team {
  trait HasTeam { val team: Team }

  case object White extends Team

  case object Black extends Team
}
