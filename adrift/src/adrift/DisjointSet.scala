package adrift

import scala.collection.mutable

// Disjoint Set aka Union-Find
class DisjointSet[T] {
  private val parents = mutable.Map.empty[T, T]
  private val children = mutable.Map.empty[T, mutable.Set[T]]
  private val roots = mutable.Set.empty[T]
  private val sizes = mutable.Map.empty[T, Int]

  def makeSet(n: T): Unit = {
    if (!contains(n)) {
      parents(n) = n
      children(n) = mutable.Set.empty
      roots.add(n)
      sizes(n) = 1
    }
  }

  def members: Iterable[T] = parents.keys

  private def recChildren(n: T): Set[T] = {
    children(n).toSet + n ++ children(n).flatMap(recChildren)
  }
  def component(n: T): Iterable[T] =
    find(n) match {
      case Some(root) => recChildren(root)
      case None => Iterable.empty
    }

  def contains(n: T): Boolean = parents contains n

  def find(n: T): Option[T] = {
    if (!contains(n)) return None
    var x = n
    while (parents(x) != x) {
      val parent = parents(x)
      parents(x) = parents(parent)
      x = parent
    }
    Some(x)
  }

  def union(u: T, v: T): Unit = {
    val xP = find(u)
    val yP = find(v)
    if (xP.isEmpty || yP.isEmpty) return
    if (xP == yP) return

    val (Some(x), Some(y)) = if (sizes(xP.get) < sizes(yP.get)) (yP, xP) else (xP, yP)
    parents(y) = x
    children(x) += y
    roots.remove(y)
    sizes(x) += sizes(y)
    sizes.remove(y)
    // TODO: more sizes cleanup?
  }
}
