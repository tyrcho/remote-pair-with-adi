
object AntSolver {
  def move(ants: List[Ant], time: Float) : List[Ant] = {
    ants.map(_.move(time))
  }


  def computeCollisionTime(ant1: Ant, ant2: Ant): Option [Float] = {
    val collisionFromLeftToRight = ant1.position > ant2.position && ant1.direction == LEFT && ant2.direction == RIGHT
    val collisionFromRightToLeft = ant1.position < ant2.position && ant1.direction == RIGHT && ant2.direction == LEFT
    if(collisionFromLeftToRight || collisionFromRightToLeft)
      Some(math.abs(ant1.position - ant2.position) / (ant1.speed + ant2.speed))
    else None
  }

  def solve(stickLength: Float, ant: Ant) = {
    val distanceToGo = computeDistanceToGo(stickLength, ant.position, ant.direction)
    computeTime(distanceToGo, ant.speed)
  }

  def solve(stickLength: Float = 0, antSpeed: Float = 1, antPosition: Float = 0, direction: Direction = RIGHT): Float = {
    val ant = Ant(antPosition, direction, antSpeed)
    solve(stickLength, ant)
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

case class Ant(position: Float, direction: Direction, speed: Float){
  def move (time: Float = 0) = {

    val signum = if(direction == RIGHT) 1 else -1

    val newPosition = position + signum * (speed * time);
    copy(position = newPosition)
  }
}