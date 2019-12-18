import scala.io.Source

val filename = "input.txt"

val line = Source.fromFile(filename).getLines.toList.apply(0)
var numbers = line.split(",").map(_.toInt).toList
println(doCalc(12, 2, numbers))
println("")

def doCalc(input1: Int, input2: Int, instructions: List[Int]) : Int = {
    var updatedInstructions = instructions
    updatedInstructions = updatedInstructions.updated(1, input1)
    updatedInstructions = updatedInstructions.updated(2, input2)
    updatedInstructions = processInstruction(0, updatedInstructions)
    updatedInstructions(0)
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
