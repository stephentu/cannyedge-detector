package com.stephentu

object DisjointSet {
  class Node[Elem](val element: Elem) {
    private[DisjointSet] var parent = this
    private[DisjointSet] var rank   = 0

    def repr: Node[Elem] = find(this)
    def unionWith(that: Node[Elem]): Unit = union(this, that)
  }

  def unit[E](elem: E): Node[E] = new Node(elem)

  private def find[E](node: Node[E]): Node[E] = {
    if (node.parent == node) node
    else {
      node.parent = find(node.parent)
      node.parent
    }
  }

  private def union[E](x: Node[E], y: Node[E]): Unit = {
    val xRoot = x.repr
    val yRoot = y.repr
    if (xRoot.rank > yRoot.rank)
      yRoot.parent = xRoot
    else if (xRoot != yRoot) {
      xRoot.parent = yRoot
      if (xRoot.rank == yRoot.rank)
        yRoot.rank += 1
    }
  }
}
