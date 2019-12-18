import scala.io.Source

val filename = "input.txt"

val line = Source.fromFile(filename).getLines.toList.apply(0)
var numbers = line.split(",").map(_.toInt).toList
val answer = findSolution(0, 0, numbers)
println(answer._1 * 100 + answer._2)

def findSolution(input1: Int, input2: Int, instructions: List[Int]) : Tuple2[Int, Int] = {
    val answer = 19690720
    
    if (doCalc(input1, input2, instructions) == answer) {
        return (input1, input2) 
    } else if (input1 < 99) {
        return findSolution(input1 + 1, input2, instructions)
    }
    else if (input1 == 99 && input2 < 99) {
       return findSolution(0, input2 + 1, instructions)    
    }
    return (-1, -1) 
}
def doCalc(input1: Int, input2: Int, instructions: List[Int]) : Int = {
    var updatedInstructions = instructions
    updatedInstructions = updatedInstructions.updated(1, input1)
    updatedInstructions = updatedInstructions.updated(2, input2)
    updatedInstructions = processInstruction(0, updatedInstructions)
    println(s"input1: $input1 input2: $input2 answer: ${updatedInstructions(0)}")
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
