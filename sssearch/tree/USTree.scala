package sssearch.tree

import sssearch._

object USTree {
	var text : String = ""
	var linkNode: Node = null

	def setLinkNode(link: Node) =
		linkNode = link

	def setText(txt: String) =
		text = txt
}

class USTree {

	var root: Node = null
	var terminalSymbol: Char = '$'

	// initiate tree construction
	def construct(txt: String): Unit = {
		txt.foreach(add)
		add(terminalSymbol)
		root.processLeafCount
	}

	def add(letter: Char): Unit = {
		val suffix: String = text
		setText(text + letter)
		if (root == null) {
			root = new Node(-1)
			root.processEnd(letter, 0)
		} else {
			root.read(suffix.toList, letter, 0)
		}
	}

	def setTerminalSymbol(symbol: Char) =
		terminalSymbol = symbol

	def print =
		root.print()
}
