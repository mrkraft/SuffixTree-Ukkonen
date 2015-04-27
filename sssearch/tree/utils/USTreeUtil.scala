package sssearch.tree.utils

import sssearch.tree.{Edge, Node}

import scala.collection.mutable.ListBuffer

object USTreeUtil {

	def getTopKSubstringByScore(root: Node, k: Int): List[(String, Int)] = {
		var result: ListBuffer[(String, Int)] = ListBuffer()
		var sortedEdgeList: ListBuffer[(String, Edge)] = ListBuffer()

		// find all edges (horizontal traversing)
		def getEdgeList(queue: List[(String, Edge)]): Unit = queue match {
			case head :: tail =>
				sortedEdgeList += head
				if (head._2.dest.isLeaf()) {
					getEdgeList(tail)
				} else {
					val prefix: String = head._1 + head._2.getStrWord()
					getEdgeList(head._2.dest.edges.toList.map(el => (prefix, el._2)).foldRight(tail)(_ :: _))
				}
			case Nil =>
		}

		def fillResult(list: List[(String, Edge)]): Unit = list match {
			case head :: tail =>
				fillAllSubstrings(head._1, head._2.getStrWord(), head._2.dest.leafCount)
				fillResult(tail)
			case Nil =>
		}

		def fillAllSubstrings(prefix: String, edgeWord:String, count: Int): Unit = {
			if (edgeWord.nonEmpty) {
				val substring: String = prefix + edgeWord
				result.append((substring, substring.size * (count - 1)))
				fillAllSubstrings(prefix, edgeWord.substring(0, edgeWord.size - 1), count)
			}
		}

		getEdgeList(root.edges.toList.map(el => ("", el._2)))
		fillResult(sortedEdgeList.sortWith((el1, el2) => el1._2.dest.leafCount.compareTo(el2._2.dest.leafCount) > 0).toList)
		result.sortWith((el1, el2) => el1._2.compareTo(el2._2) > 0).toList.slice(0, k)
	}
}
