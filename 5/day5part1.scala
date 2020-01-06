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
        println(numbers(0))
    }
    
    def processInstruction(startPosition: Int, instructions: List[Int]): List[Int] = {
        val opCode = InstructionFactory.getIntInstruction(instructions(startPosition))
        opCode match {
            case 99 => instructions
            case 1 => processInstruction(startPosition + 4, doOp(startPosition, instructions, addOp))
            case 2 => processInstruction(startPosition + 4, doOp(startPosition, instructions, multiplyOp))
        }
    }

    def doOp(startPosition: Int, instructions: List[Int], op: (Int, Int) => Int): List[Int] = {
        val num1 = instructions(instructions(startPosition + 1))
        val num2 = instructions(instructions(startPosition + 2))
        val replacePosition = instructions(startPosition + 3)
        instructions.updated(replacePosition, op(num1, num2))
    }

    def addOp(x: Int, y: Int): Int = { x + y }
    def multiplyOp(x: Int, y: Int): Int = { x * y }

}

object InstructionFactory {
    def getIntInstruction(input: Int): Int = {input}
    def getInstruction(startPosition: Int, instructions: List[Int]): Instruction = {
        val opCode = instructions(startPosition).toString.takeRight(2).toInt    
        opCode match {
            case 1 => AddInstruction(opCode, instructions)
            case 2 => MultiplyInstruction(opCode, instructions)
            case 99 => EndInstruction()
        }
    }
}

sealed abstract class Instruction(opCode: Int, instructions: List[Int]){
    val instructionSize : Int
    val parameters = getParameters(opCode)
    
    def this() = this(0, Nil)

    private def getParameters(opCode: Int): List[Parameter] = {
        List(new Parameter(1, false))
    }

    def execute(startPosition: Int) : List[Int]
}

case class AddInstruction(opCode: Int, instructions: List[Int]) extends Instruction(opCode, instructions) {
    val instructionSize = 4
    def execute(startPosition: Int): List[Int] = {
        Nil
    }
}

case class MultiplyInstruction(opCode: Int, instructions: List[Int]) extends Instruction(opCode, instructions) {
    val instructionSize = 4
    def execute(startPosition: Int): List[Int] = {
        Nil
    }
}

case class EndInstruction() extends Instruction() {
    val instructionSize = 0
    def execute(startPosition: Int): List[Int] = {
        Nil
    }
}

class Parameter(value: Int, isImmediate: Boolean)