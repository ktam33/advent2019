import scala.io.Source
import scala.annotation.meta.param

object AdventComputer {
    def main(args: Array[String]) = {
        val filename = "input.txt"
        val line = Source.fromFile(filename).getLines.toList.apply(0)
        var numbers = line.split(",").map(_.toInt).toList
        numbers = processInstruction(0, numbers)
    }
    
    def processInstruction(startPosition: Int, instructions: List[Int]): List[Int] = {
        val instruction = InstructionFactory.getInstruction(startPosition, instructions)
        instruction match {
            case EndInstruction() => instruction.execute(instructions)
            case AddInstruction(_) | MultiplyInstruction(_) | InputInstruction(_,_) | OutputInstruction(_) 
                => processInstruction(startPosition + instruction.instructionSize, instruction.execute(instructions))
        }
    }
}

object InstructionFactory {
    def getIntInstruction(input: Int): Int = {input}
    def getInstruction(startPosition: Int, instructions: List[Int]): Instruction = {
        val opCode = instructions(startPosition).toString
        val paddedOpCode = ("0" * (5 - opCode.size)) + opCode

        val instructionCode = instructions(startPosition).toString.takeRight(2).toInt    
        instructionCode match {
            case 1 => AddInstruction(getParameters(paddedOpCode.take(3), startPosition, instructions))
            case 2 => MultiplyInstruction(getParameters(paddedOpCode.take(3), startPosition, instructions))
            case 3 => InputInstruction(List(instructions(startPosition + 1)), 1) 
            case 4 => OutputInstruction(getParameters(paddedOpCode.slice(2,3), startPosition, instructions))
            case 99 => EndInstruction()
        }
    }

    def getParameters(parameterCode: String, startPosition:Int, instructions: List[Int]) : List[Int] = {
        if (parameterCode.size == 1) {
            return if (parameterCode == "1") List(instructions(startPosition + 1)) else List(instructions(instructions(startPosition + 1)))
        }
        return parameterCode.reverse.zipWithIndex.map({case (code, index) => {
            if (code == '1' || index == 2) {
                instructions(startPosition + 1 + index) 
            } else {
                instructions(instructions(startPosition + 1 + index))
            }
        }}).toList
    }
}

sealed abstract class Instruction(val parameters: List[Int]){
    val instructionSize : Int
    def this() = this(Nil)
    def execute(instructions: List[Int]) : List[Int]
}

case class AddInstruction(override val parameters: List[Int]) extends Instruction(parameters) {
    val instructionSize = 4
    def execute(instructions: List[Int]): List[Int] = {
        instructions.updated(parameters(2), parameters(0) + parameters(1))
    }
}

case class MultiplyInstruction(override val parameters: List[Int]) extends Instruction(parameters) {
    val instructionSize = 4
    def execute(instructions: List[Int]): List[Int] = {
        instructions.updated(parameters(2), parameters(0) * parameters(1))
    }
}

case class InputInstruction(override val parameters: List[Int], val input: Int) extends Instruction(parameters) {
    val instructionSize = 2
    def execute(instructions: List[Int]): List[Int] = {
        instructions.updated(parameters(0), input)
    }
}

case class OutputInstruction(override val parameters: List[Int]) extends Instruction(parameters) {
    val instructionSize = 2
    def execute(instructions: List[Int]): List[Int] = {
        println(parameters(0))
        instructions
    }
}

case class EndInstruction() extends Instruction() {
    val instructionSize = 0
    def execute(instructions: List[Int]): List[Int] = {
        instructions
    }
}
