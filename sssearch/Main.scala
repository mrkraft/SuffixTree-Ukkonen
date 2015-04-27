package sssearch

import sssearch.tree.utils.USTreeUtil
import sssearch.tree.{USTree}

object Main {

	def main(args: Array[String]) {

		val tree: USTree = new USTree
		tree.construct("xabxacabc")
		tree.print

		println("\nTop K substring by score")
		println(USTreeUtil.getTopKSubstringByScore(tree.root, 5).toString())
	}
}
