import scala.io.Source

val filename = "input.txt"

val line = Source.fromFile(filename).getLines.toList.apply(0)
var numbers = line.split(",").map(_.toInt)
numbers = numbers.updated(1, 12)
numbers = numbers.updated(2, 2)
numbers = processInstruction(0, numbers)
numbers.foreach(x => print(x + " "))
println("")

def processInstruction(startPosition: Int, x: List[Int]): List[Int] = {
    List(1,2,3)
}