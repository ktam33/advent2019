import scala.io.Source

val filename = "input.txt"

val input = Source.fromFile(filename).getLines.toList
val wire1 = input(0).split(",").toList
val wire2 = input(1).split(",").toList
println(wire1)
println(wire2)

println(getPointsForLine((1,1), "U10"))

def getPoints(directions: List[String]): List[Tuple2[Int, Int]] = {
    val startPoint = (0,0) 
    List((1,1))
}

def getPointsForLine(startPoint: Tuple2[Int,Int], instruction: String): List[Tuple2[Int,Int]] = {
    val direction = instruction.take(1)
    val magnitude = instruction.takeRight(instruction.length - 1).toInt
    List((magnitude, magnitude))
}