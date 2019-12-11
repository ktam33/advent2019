import scala.io.Source

val filename = "input.txt"

val line = Source.fromFile(filename).getLines.toList.apply(0)

val numbers = line.split(",").map(_.toInt)
println(numbers(0))

println(numbers.getClass)