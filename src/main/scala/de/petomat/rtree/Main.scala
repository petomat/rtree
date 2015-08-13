package de.petomat.rtree

object Main extends App {

  // based on petomat/geom
  import de.petomat.geom._

  // define our custom element type to be inserted into the rtree
  case class Shape(name: String, r: Rectangle) { override def toString = name }

  // some elements to work with
  val A = Shape("A", Rectangle(Point(1, 1), Size(2, 3)))
  val B = Shape("B", Rectangle(Point(4, 2), Size(3, 3)))
  val C = Shape("C", Rectangle(Point(2, 2), Size(3, 6)))

  // create an empty tree for elements of type shape
  val t0 = RTree.default.Tree.empty[Shape]

  // insert an element which creates a new tree
  val t1 = t0.insert(A, boundingBox = A.r)

  // to simplify insertion/deletion we place an implicit to extract the boundingBox
  object Shape {
    implicit val shapeRecExtractor: Shape => Rectangle = _.r
  }

  // insert and delete simplified
  val t2 = t0 insert A insert B insert C delete B
  assert(t2 == (t0 insert A insert C))

  // on the other hand we can do range-queries
  val result: Seq[Shape] = t2 search Rectangle(Point(4, 2), Size(1, 3)) // Vector(C)
  
}

