package sssearch.tree

import sssearch._
import scala.collection.immutable.HashMap

class Node(idx: Int, p: Node = null) {
	var index: Int = idx;
	var edges: Map[Char, Edge] = HashMap()
	var parent: Node = p
	var link: Node = null
	var leafCount: Int = 0

	// main processing method
	def read(suffix: List[Char], letter: Char, sufIndex: Int): Unit = {
		if (suffix.nonEmpty) {
			val edge: Option[Edge] = edges.get(suffix.head)
			if (edge.nonEmpty) {
				val nextNodeInfo: (Node, List[Char]) = edge.get.readSuffix(letter, suffix, sufIndex)
				nextNodeInfo._1.processLink(nextNodeInfo._2, letter, sufIndex + (suffix.size - nextNodeInfo._2.size))
			}
		} else {
			processEnd(letter, sufIndex)
			addLink()
		}

	}

	// check link case
	def processLink(suffix: List[Char], letter: Char, sufIndex: Int): Unit = {
		if (link != null) {
			link.read(suffix, letter, sufIndex)
		} else {
			if (suffix.nonEmpty) {
				read(suffix.tail, letter, sufIndex + 1)
			}
		}
	}

	// add one symbol edge if needed (inner node case only)
	def processEnd(letter: Char, sufIndex: Int): Node = {
		if (edges.get(letter).isEmpty) {
			addEdge(letter, text.size - 1, sufIndex)
		}
		parent
	}

	def addEdge(letter: Char, start: Int, sufIndex: Int): Unit = {
		edges += letter -> new Edge(start, start + 1, new Node(sufIndex, this))
	}

	def addEdgeWithNode(letter: Char, start: Int, end: Int, node: Node): Unit = {
		node.parent = this
		edges += letter -> new Edge(start, end, node)
	}

	def getEdgeForLetter(letter: Char): Edge = {
		edges.get(letter).get
	}

	def addLink(): Unit = {
		if (linkNode != null) {
			linkNode.link = this
		}
		if (this.parent != null){
			setLinkNode(this)
		} else {
			setLinkNode(null)
		}
	}

	def processLeafCount: Int = {
		if (isLeaf())
			return 1
		leafCount = edges.map((el) => el._2.dest.processLeafCount).fold(0) (_ + _)
		leafCount
	}

	def isLeaf(): Boolean = {
		index != -1
	}

	def isRoot(): Boolean = {
		parent == null
	}

	def print(msg: String = ""): Unit = {
		if (isLeaf()) {
			println(msg + index)
		} else {
			edges.foreach(_._2.print(msg))
		}
	}
}
