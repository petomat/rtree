package de.petomat.rtree
import de.petomat.geom._
import scala.annotation.tailrec

object RTree {
  val default: RTree = new RTree(m = 2, M = 4) {} // TODO optimize code for performance and find best values for m and M
}

abstract class RTree(protected val m: Int, protected val M: Int) { // min and max elems per node/leaf(except leaf-root)
  require(m >= 2)
  require(m <= M / 2)

  sealed trait HasBoundingBox {
    def boundingBox: Rectangle
  }

  object Tree {
    def empty[T]: Tree[T] = Leaf(Vector.empty)
    private case class Bundle[B <: HasBoundingBox](seq: Seq[B]) {
      lazy val boundingBox: Rectangle = seq.view.map(_.boundingBox).boundingBox
      def :+(b: B) = copy(seq :+ b)
      def size = seq.length
    }
    private[RTree] def split[B <: HasBoundingBox, Y](s: Seq[B]): (Seq[B], Seq[B]) = goodSplit(s)
    private def badSplit[B <: HasBoundingBox, Y](s: Seq[B]): (Seq[B], Seq[B]) = (s take (s.size - s.size / 2), s drop (s.size - s.size / 2)) // very bad split
    private def goodSplit[B <: HasBoundingBox, Y](s: Seq[B]): (Seq[B], Seq[B]) = {
      def filterOut(bs: B*)(s: Seq[B]): Seq[B] = { val bsSet = bs.toSet; s filterNot bsSet }
      def boundingBox(s: Seq[B]): Rectangle = s.view.map(_.boundingBox).boundingBox
      def candidate(boundingBox: Rectangle)(s: Seq[B]): B = selectCandidate(Selectors.minEnlargementWrt(boundingBox))(s)
      def enlargement(candidate: B, baseBoundingBox: Rectangle): Size.Area = Set(baseBoundingBox, candidate.boundingBox).boundingBox.area - baseBoundingBox.area
      @tailrec def assigning(remaining: Seq[B], left: Bundle[B], right: Bundle[B]): (Seq[B], Seq[B]) = {
        // println("ASSIGN:")
        // println("remaining=" + remaining)
        // println("left=" + left)
        // println("right=" + right)
        remaining match {
          case Nil                             => (left.seq, right.seq)
          case rs if rs.size == m - left.size  => (left.seq ++ rs, right.seq)
          case rs if rs.size == m - right.size => (left.seq, right.seq ++ rs)
          case _ =>
            // TODO this can be done faster, but with ugly code
            val candidateLeft = candidate(left.boundingBox)(remaining)
            val candidateRight = candidate(right.boundingBox)(remaining)
            // TODO deadspace criteria instead of minEnlargement
            val leftEnlargementByCandidateLeft = enlargement(candidateLeft, left.boundingBox)
            val rightEnlargementByCandidateRight = enlargement(candidateRight, right.boundingBox)
            // println("candidateLeft=" + candidateLeft)
            // println("candidateRight=" + candidateRight)
            // println("leftEnlargementByCandidateLeft=" + leftEnlargementByCandidateLeft)
            // println("rightEnlargementByCandidateRight=" + rightEnlargementByCandidateRight)
            if (leftEnlargementByCandidateLeft <= rightEnlargementByCandidateRight) { // TODO random if equals?
              assigning(filterOut(candidateLeft)(remaining), left :+ candidateLeft, right)
            } else assigning(filterOut(candidateRight)(remaining), left, right :+ candidateRight)
        }
      }
      // Choose two objects as seeds for the two nodes, where these
      // objects if put together create as much dead space as possible (dead space
      // is the space that remains from the MBR if the areas of the two objects
      // are ignored).
      assert(s.size >= 2)
      val combisWithDeadSpaceArea = s combinations 2 map { case xs @ Seq(x1, x2) => (x1, x2, Seq(x1.boundingBox, x2.boundingBox).deadSpaceArea()) }
      val maxSortedDeadSpaceAreaCombis = combisWithDeadSpaceArea.toSeq.sortBy(_._3)(implicitly[Ordering[Size.Area]].reverse) // descendingly sorted by ordering.reverse
      val (seed1, seed2, _) = maxSortedDeadSpaceAreaCombis.head
      // println("seed1=" + seed1)
      // println("seed2=" + seed2)
      // Assign remaining to a group with seed 1 or 2 depending on minimum area enlargement
      assigning(remaining = filterOut(seed1, seed2)(s), Bundle(Seq(seed1)), Bundle(Seq(seed2)))
    }
    private[RTree] object Selectors {
      private implicit val sizeOrd: Ordering[Size] = Ordering by { _.area }
      private def extremesBy[X, Y](xs: Seq[X])(f: X => Y)(implicit ord: Ordering[Y]): Seq[X] = {
        xs match {
          case Nil => Nil
          case _ =>
            val sorted = xs sortBy f // TODO own copy free sort function
            val y = f(sorted.head)
            sorted takeWhile { f(_) == y }
        }
      }
      private def minisBy[X, Y](xs: Seq[X])(f: X => Y)(implicit ord: Ordering[Y]): Seq[X] = extremesBy(xs)(f)
      private def maxisBy[X, Y](xs: Seq[X])(f: X => Y)(implicit ord: Ordering[Y]): Seq[X] = extremesBy(xs)(f)(ord.reverse)
      def maxOverlapWith[B <: HasBoundingBox](r: Rectangle): Seq[B] => Seq[B] = { maxisBy(_) { _.boundingBox.intersection(r).size } }
      def minEnlargementWrt[B <: HasBoundingBox](r: Rectangle): Seq[B] => Seq[B] = { minisBy(_) { c => Set(c.boundingBox, r).boundingBox.size } }
      def minArea[B <: HasBoundingBox]: Seq[B] => Seq[B] = { minisBy(_) { _.boundingBox.size } }
    }
    private[RTree] def selectCandidate[B <: HasBoundingBox](selectors: (Seq[B] => Seq[B])*)(elems: Seq[B]): B = {
      Iterator.iterate((elems, selectors))({
        case (candidates, selector +: restSelectors) => (selector(candidates), restSelectors)
        case x                                       => x
      }).dropWhile {
        case (c, s) => s.nonEmpty && c.size > 1
      }.next._1.head // TODO or random one? if stable sort, shuffle elems before iterating the iterator
    }
  }

  sealed trait Tree[T] extends HasBoundingBox { // TODO lazy hashcode reasonable? yes and equals too because of span function
    protected type Elem <: HasBoundingBox
    protected type Elems = Seq[Elem]
    protected type Selector = Elems => Elems
    protected def constructor(s: Elems): Tree[T]
    protected final def constructAndSplitIfFull(s: Elems): Seq[Tree[T]] = {
      if (s.size <= M) Seq(constructor(s))
      else { // do split
        val (left, right) = Tree.split(s)
        Seq(constructor(left), constructor(right))
      }
    }
    def search(query: Rectangle): Seq[T]
    private[RTree] def insertInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]]
    final def insert(t: T, boundingBox: Rectangle): Tree[T] = {
      // println("INSERTING " + t)
      insertInternally(t, boundingBox) match {
        case Seq(t)           => t
        case s if s.size == 2 => Node(s) // new root
      }
    }
    private[RTree] def deleteInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]]
    final def delete(t: T, boundingBox: Rectangle): Tree[T] = {
      // println("DELETING " + t)
      deleteInternally(t, boundingBox) match {
        case Nil       => Tree.empty[T]
        case Seq(root) => root
      }
    }
    final def show: String = show()
    def show(ident: String = ""): String
    final def insert(t: T)(implicit t2r: T => Rectangle): Tree[T] = insert(t, t2r(t))
    final def delete(t: T)(implicit t2r: T => Rectangle): Tree[T] = delete(t, t2r(t))
  }

  case class Node[T](children: Seq[Tree[T]]) extends Tree[T] {
    assert(children.size >= m)
    assert(children.size <= M)
    protected type Elem = Tree[T]
    protected def constructor(s: Seq[Elem]): Tree[T] = Node(s)
    val boundingBox: Rectangle = children. /*view.*/ map(_.boundingBox).boundingBox // val boundingBox: Rectangle = children. /*view.*/ map(_.boundingBox).boundingBoxOpt.getOrElse(Rectangle.empty)
    def search(query: Rectangle): Seq[T] = children /*.view*/ filter (_.boundingBox intersects query) flatMap (_ search query)
    private[RTree] def insertInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]] = {
      import Tree.Selectors._
      val candidate: Elem = Tree.selectCandidate(maxOverlapWith(boundingBox), minEnlargementWrt(boundingBox), minArea)(children)
      val (prefix, candidateAndSuffix) = children span (_ != candidate)
      val newChildren = prefix ++ candidate.insertInternally(t, boundingBox) ++ candidateAndSuffix.tail
      constructAndSplitIfFull(newChildren)
    }
    private[RTree] def deleteInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]] = {
      val newChildren = children /*.view*/ flatMap {
        case c if c.boundingBox intersects boundingBox => c.deleteInternally(t, boundingBox)
        case c                                         => Seq(c)
      }
      if (newChildren.size < m) newChildren else constructAndSplitIfFull(newChildren)
    }
    def show(ident: String) = children.map(_.show(ident + "  ")).mkString(s"${ident}Node$boundingBox}(\n${ident}", "\n", s"\n${ident})")
  }

  object Leaf {
    case class Entry[T](t: T, boundingBox: Rectangle) extends HasBoundingBox
  }

  case class Leaf[T](entries: Seq[Leaf.Entry[T]]) extends Tree[T] {
    assert(entries.size <= M)
    protected type Elem = Leaf.Entry[T]
    protected def constructor(s: Seq[Elem]): Tree[T] = Leaf(s)
    val boundingBox: Rectangle = entries.view.map(_.boundingBox).boundingBox
    def search(query: Rectangle): Seq[T] = entries collect { case Leaf.Entry(t, b) if query intersects b => t }
    private[RTree] def insertInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]] = constructAndSplitIfFull(entries :+ Leaf.Entry(t, boundingBox))
    private[RTree] def deleteInternally(t: T, boundingBox: Rectangle): Seq[Tree[T]] = {
      entries filter (_.t != t) match { // TODO also test for boundingbox?
        case Nil => Nil
        case es  => Seq(copy(entries = es))
      }
    }
    def show(ident: String) = entries /*.map(_.boundingBox)*/ .mkString(s"${ident}Leaf${boundingBox}(\n${ident}  ", s",\n${ident}  ", s"\n${ident})")
  }
}


  
  
  
              // QUADRATIC SPLIT: Choose two objects as seeds for the two nodes, where these
              // objects if put together create as much dead space as possible (dead space
              // is the space that remains from the MBR if the areas of the two objects
              // are ignored). Then, until there are no remaining objects, insert the object
              // for which the difference of dead space if assigned to each of the two nodes
              // is maximized in the node that requires less enlargement of its respective MBR.
