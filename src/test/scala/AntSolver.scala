object AntSolver {
  def solve(stickLength: Float = 0, antSpeed: Float = 1, antPosition: Float = 0, direction: Direction = RIGHT): Float = {
    val distanceToGo = computeDistanceToGo(stickLength, antPosition, direction)
    computeTime(distanceToGo, antSpeed)
  }

  def solve(ants: List[Ant], stickLength: Float): List[Float] =
    for {
      Ant(position, direction, speed) <- ants
    } yield solve(stickLength, speed, position, direction)


  private def computeDistanceToGo(stickLength: Float, antPosition: Float, direction: Direction) = direction match {
    case RIGHT => stickLength - antPosition
    case LEFT => antPosition
  }

  private def computeTime(distance: Float, speed: Float): Float = {
    distance / speed
  }
}


sealed trait Direction

object RIGHT extends Direction

object LEFT extends Direction

case class Ant(position: Float, direction: Direction, speed: Float)