package example

sealed trait Tree[+A]
case class Leaf[A]( value: A ) extends Tree [A]
case class Branch [A] ( left:Tree[A] , right:Tree [A] ) extends Tree [A]

object Tree extends App{

  def size [A] ( tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => size(left) + size(right) + 1
  }


  println(size(Leaf(20)))
  println(size(Branch(Leaf(10),Leaf(20))))
  println(size(Branch(Branch(Leaf(20),Leaf(20)),Leaf(20))))
  println(size(Branch(Branch(Branch(Leaf(20),Leaf(10)),Leaf(20)),Leaf(20))))


  def depth [A] (tree: Tree [A]) : Int = ???

}