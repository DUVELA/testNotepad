//code - https://github.com/MartinRosenberg/ScalaExercises/tree/master/src/main/scala/com/martinbrosenberg/exercises/exercism/track/e09_robotsimulator

object Direction extends Enumeration{
    type Direction = Value

    val L = Value(-1)
    val R = Value(1)
}

object Bearing extends Enumeration {
    import Direction._

    type Bearing = Value

    val North, East, South, West = Value
    def ddt(be: Bearing, dir: Direction): Bearing = Bearing(be.id + dir.id match {
        case -1 => 3
        case 4 => 0
        case x => x
    })
}

// ========== ========== ========== ========== ========== ========== ========== ==========

import Direction._
import Bearing._
case class Robot(bearing: Bearing, coordinates: (Int, Int))  {

    def advance: Robot = coordinates match { 
        case (x, y) => copy(coordinates = bearing match {
            case North => (x, y + 1)
            case East  => (x + 1, y)
            case South => (x, y - 1)
            case West  => (x - 1, y)
        })
    }

    private def turn(dir: Direction): Robot = copy(Bearing.ddt(bearing, dir))
    def turnLeft: Robot = turn(L)
    def turnRight: Robot = turn(R)

    def simulate(commands: String): Robot =
    commands./:(this)((robot, cmd) => cmd match {
        case 'A' => robot.advance
        case 'L' => robot.turnLeft
        case 'R' => robot.turnRight
    })

}

/* 
- object does not take parameters. - Robot(Bearing.East, (2, -7))
- ???(a: A, b: B).???.???  - ??
- copy, class

apply - https://blog.matthewrathbone.com/2017/03/06/scala-object-apply-functions.html
    if != apply - too many arguments (2) for method apply: (x: Int)Bearing.Value in class Enumeration (Bearing.ddt)
    
    val North, East, South, West = Value
    Bearing.North.id ~ Bearing.West.id = North.id: 0, East.id: 1, South.id: 2, West.id: 3
    case 4 => 0 (Bearing.West.id + Direction.R.id)
*/
