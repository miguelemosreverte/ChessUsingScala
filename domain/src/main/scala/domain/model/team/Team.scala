package domain.model.team

sealed trait Team

object Team {
  case object White extends Team
  case object Black extends Team
}
