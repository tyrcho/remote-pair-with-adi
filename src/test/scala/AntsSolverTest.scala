import org.scalatest.{FlatSpec, Matchers}

class AntsSolverTest extends FlatSpec with Matchers {
  behavior of "AntsSolver"

  it should "check that time is 0 for no ants" in {
    val time = AntSolver.solve()
    time shouldBe 0
  }

  it should "check that time is 0 for an ant on a zero-length stick" in {
    val stickLength = 0
    val time = AntSolver.solve(stickLength)
    time shouldBe 0
  }

  it should "check that time is 1 for an ant on a stick with length 1" in {
    val stickLength = 1
    val time = AntSolver.solve(stickLength)
    time shouldBe 1
  }

  it should "check that time is 1 for an ant with velocity 2 on a stick with length 2" in {
    val stickLength = 2
    val antSpeed = 2
    val time = AntSolver.solve(stickLength, antSpeed)
    time shouldBe 1
  }

  it should "check that time is 1.5 for an ant with velocity 2 on a stick with length 3" in {
    val stickLength = 3
    val antSpeed = 2
    val time = AntSolver.solve(stickLength, antSpeed)
    time shouldBe 1.5F +- 0.00001F
  }

  it should "check that time is 0.5 for an ant with velocity 2 and position 1 on a stick with length 2" in {
    val stickLength = 2
    val antSpeed = 2
    val antPosition = 1
    val time = AntSolver.solve(stickLength, antSpeed, antPosition)
    time shouldBe 0.5F +- 0.00001F
  }

  it should "check that time is 0.5 for an ant with velocity 1 and position 1.5 on a stick with length 2" in {
    val stickLength = 2
    val antSpeed = 1
    val antPosition = 1.5F
    val time = AntSolver.solve(stickLength, antSpeed, antPosition)
    time shouldBe 0.5F +- 0.00001F
  }

  it should "check that time is 0 for an ant with velocity 1, facing right and position 1 on a stick with length 1" in {
    val stickLength = 1
    val antSpeed = 1
    val antPosition = 1F
    val time = AntSolver.solve(stickLength, antSpeed, antPosition, RIGHT)
    time shouldBe 0F +- 0.00001F
  }

  it should "check that time is 1 for an ant with velocity 1, facing left and position 1 on a stick with length 1" in {
    val stickLength = 1
    val antSpeed = 1
    val antPosition = 1F
    val time = AntSolver.solve(stickLength, antSpeed, antPosition, LEFT)
    time shouldBe 1F +- 0.00001F
  }

  it should "check that time is 1.5 for an ant with velocity 1, facing left and position 2.5 on a stick with length 5" in {
    val stickLength = 5
    val antSpeed = 1
    val antPosition = 2.5F
    val time = AntSolver.solve(stickLength, antSpeed, antPosition, LEFT)
    time shouldBe 2.5F +- 0.00001F
  }

  it should "check that time is 2 for an ant with velocity 2, facing right and position 2 on a stick with length 6" in {
    val stickLength = 6
    val antSpeed = 2
    val antPosition = 2
    val time = AntSolver.solve(stickLength, antSpeed, antPosition, RIGHT)
    time shouldBe 2F +- 0.00001F
  }

  it should "check that times are (3,2) for 2 ants with velocity 1 and 1, facing RIGHT and positions 0 and 1 on a stick with length 3" in {
    val ant1 = Ant(speed = 1, direction = RIGHT, position = 0)
    val ant2 = Ant(speed = 1, direction = RIGHT, position = 1)
    val ants = List(ant1, ant2)
    val stickLength = 3

    val times = AntSolver.solve(ants, stickLength)

    times(0) shouldBe 3F +- 0.00001F
    times(1) shouldBe 2F +- 0.00001F
  }

  it should "check that times are (4,2) for 2 ants with velocity 1 and 1, facing RIGHT and positions 0 and 2 on a stick with length 4" in {
    val ant1 = Ant(speed = 1, direction = RIGHT, position = 0)
    val ant2 = Ant(speed = 1, direction = RIGHT, position = 2)
    val ants = List(ant1, ant2)
    val stickLength = 4

    val times = AntSolver.solve(ants, stickLength)

    times(0) shouldBe 4F +- 0.00001F
    times(1) shouldBe 2F +- 0.00001F
  }

  it should "check that ants collide when they have same speed" in {
    val ant1 = Ant(speed = 1, direction = RIGHT, position = 0)
    val ant2 = Ant(speed = 1, direction = LEFT, position = 2)
    val ants = List(ant1, ant2)
    val stickLength = 2

    val times = AntSolver.solve(ants, stickLength)

    times(0) shouldBe 2F +- 0.00001F
    times(1) shouldBe 2F +- 0.00001F
  }

//  it should "check that ants collide when they have different speed" in {
//    val ant1 = Ant(speed = 1, direction = RIGHT, position = 0)
//    val ant2 = Ant(speed = 2, direction = LEFT, position = 3)
//    val ants = List(ant1, ant2)
//    val stickLength = 4
//
//    val times = AntSolver.solve(ants, stickLength)
//
//    times(0) shouldBe 2F +- 0.00001F
//    times(1) shouldBe 2.5F +- 0.00001F
//  }
}
