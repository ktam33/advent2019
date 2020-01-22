import scala.io.Source
import scala.annotation.meta.param

object AdventComputer {
    def main(args: Array[String]) = {
        val filename = "input.txt"
        val lines = Source.fromFile(filename).getLines.toList
        val allNodes = lines.map(line => {
            val lineParts = line.split("\\)")
            new Node(lineParts(1), lineParts(0))
        })
        allNodes.foreach(n => n.setNextNode(allNodes))
        allNodes.foreach(n => n.setIndirectNodes())
        println(allNodes)
        allNodes.foreach(n => {
            println(if (n.nextNode.isEmpty) n.id + " end" else n.id + " " + n.nextNode.get.id )
            println(n.nextNode.isEmpty)
            println(n.indirectNodes)
        })
        println(countAll(allNodes))
    }

    def countAll(nodes: List[Node]): Int = {
        var count = nodes.size
        nodes.foreach(n => {
            count = count + n.indirectNodes.size
        })
        count 
    }
}

class Node(val id: String, val nextNodeId: String) {
    var nextNode = None : Option[Node]
    var indirectNodes = List[Node]() 
    def setNextNode(allNodes: List[Node]) = {
        val matchingNodes = allNodes.filter(n => n.id == nextNodeId)
        if (matchingNodes.size > 0) {
            nextNode = Some(matchingNodes.head)
        }
    }

    def setIndirectNodes() = {
        var currentNode = nextNode
        while(currentNode.nonEmpty){
            indirectNodes = currentNode.get :: indirectNodes
            currentNode = currentNode.get.nextNode
        }
    }
}
