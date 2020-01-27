import scala.io.Source
import scala.annotation.tailrec

object AdventComputer {
    def main(args: Array[String]) = {
        val filename = "input.txt"
        val lines = Source.fromFile(filename).getLines.toList
        var allNodes = lines.map(line => {
            val lineParts = line.split("\\)")
            new Node(lineParts(1), lineParts(0))
        })
        val terminalNodes = allNodes.filter(n => n.nextNode.isEmpty).map(n => new Node(n.nextNodeId, ""))
        allNodes = allNodes ::: terminalNodes
        allNodes.foreach(n => n.setNextNode(allNodes))
        // subtracting 2 because the jump from YOU and SAN to what they orbit don't count
        println(findShortestPathLength(allNodes, "YOU", "SAN") - 2)
    }


    def findShortestPathLength(nodes: List[Node], start: String, end: String): Int = {
        val currentNode = nodes.filter(n => n.id == start).head
        var nodesToSearch = List[Node]()
        var visitedNodes = List[Node](currentNode)
        nodesToSearch = currentNode.addAdjacentNodes(visitedNodes)
        return searchNodes(nodesToSearch, visitedNodes, end, 1) 
    }

    @tailrec def searchNodes(nodes: List[Node], visitedNodes: List[Node], end: String, currentLevel: Int) : Int = {
        if (nodes.exists(n => n.id == end)) return currentLevel
        val newVisitedNodes = visitedNodes ::: nodes
        var nextNodes = List[Node]()
        nodes.foreach(n => {
            /*
            if (n.id == end){
                return currentLevel
            } 
            */
            nextNodes = nextNodes ::: n.addAdjacentNodes(visitedNodes)
        })
        if (nextNodes.size == 0) return -1
        return searchNodes(nextNodes, newVisitedNodes, end, currentLevel + 1)
    }
}

class Node(val id: String, val nextNodeId: String) {
    var nextNode = None : Option[Node]
    var previousNodes = List[Node]()
    
    override def toString(): String = this.id + " : " + this.nextNodeId

    def setNextNode(allNodes: List[Node]) = {
        val matchingNodes = allNodes.filter(n => n.id == nextNodeId)
        if (matchingNodes.size > 0) {
            nextNode = Some(matchingNodes.head)
            nextNode.get.previousNodes = this :: nextNode.get.previousNodes
        }
    }

    def addAdjacentNodes(visitedNodes: List[Node]): List[Node] = {
        var nextNodes = List[Node]()
        if (this.nextNode.nonEmpty && !visitedNodes.exists(n => n.id == this.nextNode.get.id)) {
            nextNodes = this.nextNode.get :: nextNodes
        }
        this.previousNodes.foreach(n => {
            if(!visitedNodes.exists(p => p.id == n.id)) {
                nextNodes = n :: nextNodes
            } 
        })
        nextNodes
    }
}
