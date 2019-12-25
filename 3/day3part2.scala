import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toList
val wire1 = input(0).split(",").toList
val wire2 = input(1).split(",").toList
//println(wire1)
//println(wire2)
val wire1Points = getPoints((0,0,0), wire1)
val wire2Points = getPoints((0,0,0), wire2)
println(wire2Points.sortWith(_._3 < _._3))
//val commonPoints = wire1Points.intersect(wire2Points)
//println(commonPoints.sortWith(_._3 > _._3))

def getPoints(startPoint: Tuple3[Int, Int, Int], directions: List[String]): List[Tuple3[Int, Int, Int]] = {
    var result =List[Tuple3[Int, Int, Int]]()
    var lastPoint = startPoint
    directions.foreach(direction => {
        var linePoints = getPointsForLine(lastPoint, direction)
        result = result ++ linePoints
        lastPoint = linePoints(0)
    })
    result
}

def getPointsForLine(startPoint: Tuple3[Int,Int,Int], instruction: String): List[Tuple3[Int,Int,Int]] = {
    val direction = instruction.take(1)
    val magnitude = instruction.takeRight(instruction.length - 1).toInt
    var result = List[Tuple3[Int, Int, Int]]()
    for (i <- 1 to magnitude) {
        val lastPoint = if (i == 1) startPoint else result(0)
        direction match {
            case "U" => result = (lastPoint._1, lastPoint._2 + 1, lastPoint._3 + 1) :: result
            case "D" => result = (lastPoint._1, lastPoint._2 - 1, lastPoint._3 + 1) :: result
            case "L" => result = (lastPoint._1 - 1, lastPoint._2, lastPoint._3 + 1) :: result
            case "R" => result = (lastPoint._1 + 1, lastPoint._2, lastPoint._3 + 1) :: result
        }
    }
    result
}