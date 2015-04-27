import sssearch.tree._

package object sssearch {

	def text = USTree.text

	def linkNode = USTree.linkNode

	def setLinkNode(link: Node) =
		USTree.setLinkNode(link)

	def setText(txt: String) =
		USTree.setText(txt)
}
