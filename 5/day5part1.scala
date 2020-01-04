import scala.io.Source

object AdventComputer {
    def main(args: Array[String]) = {
        println("Hello, " + args(0))
        val filename = "input.txt"
        val line = Source.fromFile(filename).getLines.toList.apply(0)
        var numbers = line.split(",").map(_.toInt).toList
        numbers = numbers.updated(1, 12)
        numbers = numbers.updated(2, 2)
        numbers = processInstruction(0, numbers)
        //numbers.foreach(x => print(x + " "))
        //:println("")
        println(numbers(0))
    }
    
    def processInstruction(startPosition: Int, instructions: List[Int]): List[Int] = {
        val opCode = instructions(startPosition)
        opCode match {
            case 99 => instructions
            case 1 => processInstruction(startPosition + 4, doOp(startPosition, instructions, (x, y) => x + y))
            case 2 => processInstruction(startPosition + 4, doOp(startPosition, instructions, (x, y) => x * y))
        }
    }

    def doOp(startPosition: Int, instructions: List[Int], op: (Int, Int) => Int): List[Int] = {
        val num1 = instructions(instructions(startPosition + 1))
        val num2 = instructions(instructions(startPosition + 2))
        val replacePosition = instructions(startPosition + 3)
        instructions.updated(replacePosition, op(num1, num2))
    }
}