import scala.io.Source

if (args.length > 0) {
  val lines = Source.fromFile(args(0)).getLines().toList
  println("First answer: " + lines.map((x) => calcFuel(x.toInt)).sum)
  println("Second answer: " + lines.map((x) => calcFuel2(x.toInt)).sum)
}

def calcFuel(x: Int) = x / 3 - 2

def calcFuel2(x: Int): Int = {
  val currentFuel = x /3 - 2
  if (currentFuel <= 0)
    return 0
  return currentFuel + calcFuel2(currentFuel)  
}