package sssearch.tree

import sssearch._

class Edge(val s: Int, val e: Int, val node: Node) {
	var start: Int = s
	var end: Int = e
	var dest: Node = node


	def readSuffix(letter: Char, suffix: List[Char], sufIndex: Int): (Node, List[Char]) = {
		val edgeWord: List[Char] = getWord()

		if (edgeWord.size == suffix.size) {
			(processNode(letter, sufIndex), suffix)
		} else if (edgeWord.size < suffix.size) {
			val suffixPart = suffix.slice(edgeWord.size, suffix.size)
			dest.getEdgeForLetter(suffixPart.head).readSuffix(letter, suffixPart, sufIndex)
		} else {	// in the middle of edge
			if (edgeWord(suffix.size) != letter) {
				(processInEdgeCase(letter, suffix, sufIndex), suffix)
			} else {
				(dest.parent, suffix.tail)
			}
		}
	}

	// stay at node
	def processNode(letter: Char, sufIndex: Int): Node = {
		if (dest.isLeaf()) {
			end += 1
			dest.parent
		} else {
			dest.processEnd(letter, sufIndex)
		}
	}

	// stay in the middle of edge
	def processInEdgeCase(letter: Char, suffix: List[Char], sufIndex: Int): Node = {
		val middleDest: Node = new Node(-1, dest.parent)
		middleDest.addEdgeWithNode(text(start + suffix.size), start + suffix.size, end, dest)
		middleDest.addEdge(letter, text.size - 1, sufIndex)
		dest = middleDest
		end = start + suffix.size
		middleDest.addLink()

		middleDest.parent
	}

	def getWord(): List[Char] = {
		text.substring(start, end).toList
	}

	def getStrWord(): String = {
		text.substring(start, end)
	}

	def print(msg: String): Unit = {
		dest.print(msg + text.substring(start, end))
	}
}
