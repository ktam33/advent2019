import scala.io.Source
import scala.annotation.meta.param

object AdventComputer {
    def main(args: Array[String]) = {
        val filename = "input_test.txt"
        val lines = Source.fromFile(filename).getLines.toList
        val allNodes = lines.map(line => {
            val lineParts = line.split("\\)")
            new Node(lineParts(0), lineParts(1))
        })
        allNodes.foreach(n => n.setNextNode(allNodes))
        println(allNodes)
        allNodes.foreach(n => println(if (n.nextNode.isEmpty) "end" else n.nextNode.get.id ))
    }
}

class Node(val id: String, val nextNodeId: String) {
    var nextNode = None : Option[Node]
    def setNextNode(allNodes: List[Node]) = {
        val matchingNodes = allNodes.filter(n => n.id == nextNodeId)
        if (matchingNodes.size > 0) {
            nextNode = Some(matchingNodes.head)
        }
    }
}
