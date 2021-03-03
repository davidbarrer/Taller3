package example

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h:A,t:List[A]) extends List[A]


object List extends App {
  // A* seq [A]
  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
  println(length(List(1,2,3)))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(h, t) => h + sum(t)

  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Const(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }


  //Remueve el primer valor de una lista dada tipo List
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(h, t) => t
  }


  //Devuelve el primer valor de una lista tipo List
  def head[A](lst: List[A]): A = lst match {
    case Const(h, t) => h
  }

  //Recibe una lista de booleanos y devuelve true si todos los elementos son verdaderos, de lo contrario devuelve false
  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => true
    case Const(false, t) => false
    case Const(true, t) => and(t)
  }


  //rercibe una lista de booleanos y devuelve false si todos los elementos son falsos, de lo contrario devuelve true
  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(true, t) => true
    case Const(false, t) => or(t)
  }

  //Funcion que elimina n elementos de una lista desde, de izquierda a derecha
  def drop [A] (n:Int, lst:List[A] ) : List[A] = (n,lst) match {
    case (0,lst) => lst
    case (n, Nil) => Nil
    case (n, Const(h,t)) => drop(n-1,t)
  }


  // función que devuelve el mayor valor en una lista de valores tipo Int
  def max(lst: List[Int]): Int = {
    @tailrec
    def maxTemporal(lst: List[Int], max: Int): Int = lst match {
      case Nil => max
      case Const(h, t) => maxTemporal(t, if (h > max) h else max)
    }

    maxTemporal(tail(lst), head(lst)) //Elimina el primer valor de la lista en el primer parametro, y el segundo parametro es el primero de la lista
  }


  // función que devuelve el mínimo valor en una lista de valores tipo Long
  def min(lst: List[Long]): Long = {
    def minTemporal(lst: List[Long], min: Long): Long = lst match {
      case Nil => min
      case Const(h, t) => minTemporal(t, if (h < min) h else min)
    }

    minTemporal(tail(lst), head(lst))
  }

  //Funcion que recibe una lista de Double y devuelve el menor y mayor Double correspondientemente
  def minMax(lst: List[Double]): (Double, Double) = {
    @tailrec
    def minMaxTemporal(lst: List[Double], min: Double, max: Double): (Double, Double) = lst match {
      case Nil => (min, max)
      case Const(h, t) => minMaxTemporal(t, if (h < min) h else min, if (h > max) h else max)
    }

    minMaxTemporal(tail(lst), head(lst), head(lst))
  }
  //addEnd function: Adds an element at the tail of a given list of Any type of elements
  def addEnd[A] (n:A,lst:List[A]): List[A] = lst match {
    case Nil => Const(n,Nil)
    case Const(h,t) => Const(head(lst),addEnd(n,tail(lst)))
  }
  val list1= List(1,2,3,4,5,6,7,8)
  println(addEnd(8,list1))

  //--------------------------------------------------------------------------------------------------------------
  //List Building
  //EXERCISE 1.
  /*take function: It receives two parameters, the first is a positive Int and the second is a List of Any value
   returns the first n values of the given list, by calling recursively the take function until the n value equals 0
   */
  def take [A](n:Int,lst:List[A]): List[A] = {
    if (n>length(lst)) lst
    else if (n==0) Nil
    else  Const(head(lst),take(n-1,tail(lst)))
  }

  println(take(3,List("a","b","c","d","e")))
  println(take(0,List(1,2,3,4)))
  println(take(6,List(1.0,2.0,3.0)))


  //EXERCISE 2.
  /*init function: It receives a List of Any value as parameter
  iniTemp function inside init function, uses a pivot named n to match the length of lst minus 1 until it reaches 0
  to stop adding  elements to the new list and don't take the last element of the original list.
  returns a new list without the last value of the original List
   */
  def init [A] ( lst:List[A] ): List[A] = {
    def initTemp ( n:Int, lst:List[A] ) :List[A] = n match {
      case 0 => Nil
      case _ => Const(head(lst),initTemp((n-1),tail(lst)))
    }
    initTemp((length(lst)-1),lst)
  }

  println(init(List(1,2,3,4,5,6)))
  println(init(List(1)))


  //EXERCIE 3.
  /*split function: It receives two parameters, the first one is an Int(n) and the second one a List of Any value.
  returns a tuple with two new lists, the first one with the size of the first n elements of the original list
  and the second one with the size-n elements of the original list.

  splitTempfunction: It receives three parameters, n(the same n of split function), lst(2) which initiates as Nil, and
   lst(the same lst of split function). Split temp is called recursively to decrement the n value until it reaches 0,
   and to create two new lists, one taking value by value of the original list (n times) and the other one
   receives the remainder (tail) of the list.
   */
  def split[A](n:Int,lst:List[A]):(List[A],List[A]) = {
    @tailrec
    def splitTemp[A](n:Int,lst2:List[A],lst:List[A]):(List[A],List[A]) = (n,lst) match {
      case (0,Const(h,t)) => (lst2,lst)
      case (n,Nil) => (lst2,lst)
      case (_,Const(h,t)) => splitTemp(n-1,addEnd(h,lst2),t)
    }
    splitTemp(n,Nil,lst)
  }
  println(split(3,List(1,2,3,4,5,6,7)))
  println(split(1,List(1,2,3,4,5,6,7)))
  println(split(8,List(1,2,3,4,5,6,7)))
  println(split(0,List(1,2,3,4,5,6,7)))


  //EXERCISE 4
  /*
  zip function, receives two lists as parameters and creates a new list of the same size, but with tuples.
  Each tuple contains two elements which are the ones of the same position in the given lists.
  zip temp receives the lst1, lst2 Lists (which are the one passed in zip function) and a Nil list to initiate
  a the new list of tuples.
  zip temp is called recursively using the tail of the first list as lst1, the tail of the second list as lst2,
  and addEnd function to create the newlist of tuples which take as head the heads elements of lst1 and lst2,
  and as tail the current value of lst3.
   */
  def zip [A,B] ( lst1:List[A], lst2:List[B]): List[(A,B)] = {
    @tailrec
    def zipTemp [A,B] (lst1:List[A],lst2:List[B],lst3:List[(A,B)]): List[(A,B)] = (lst1,lst2) match {
      case (Nil, Const(h2,t2)) => lst3
      case (Const(h,t),Nil) => lst3
      case (Const(h,t),Const(h2,t2))  => zipTemp(t,t2,(addEnd((h,h2),lst3)))
    }
    zipTemp(lst1,lst2,Nil)
  }

  println(zip(List(1,2,3),List(true,false,true,true)))


  //EXERCISE 5
  /*
  unzip function: receives a list of tuples(with two elements inside of each tuple) as parameter,
  and Separates this list, creating two new lists with these elements.
  unzipTemp function receives as parameters, the original list passed, and two empty lists.
  Calling recursively the unzipTemp function, giving the t of the original list as lst, then in the second and third
  parameter the addEnd function is called to create the new two list taking the elements of each tuple and adding
  these to the lst1 and lst2 lists.
   */
  def unzip [A,B] ( lst:List[(A,B)] ): (List[A],List[B]) = {
    @tailrec
    def unzipTemp[A, B](lst: List[(A, B)], lst1: List[A], lst2: List[B]): (List[A], List[B]) = lst match{
      case Nil => (lst1,lst2)
      case (Const((h1,h2),t)) => unzipTemp(t, addEnd(h1,lst1) , addEnd(h2,lst2))
    }
    unzipTemp(lst,Nil,Nil)
  }
  println(unzip(List((1,"a"),(2,"b"),(3,"b"))))


  //EXERCISE 6
  /*
  reverse function: Receives a list as parameter and creates a new list but reverting the order of the original one.
  reverseTemp function: Receives two parameters, the original list of reverse function, and an empty list.
  Calling recursively the reverseTemp function, receives the tail of the list, and creates a new list which adds
  each new value to the tail of the list.

   */
  def reverse [A](lst:List[A]):List[A] = {
    @tailrec
    def reverseTemp[A](lst: List[A], lst1: List[A]): List[A] = lst match {
      case Nil => lst1
      case Const(h, t) => reverseTemp(t, Const(h, lst1))
    }
    reverseTemp(lst, Nil)
  }

  println(reverse(List("a","b","c")))
  println(reverse(List(1,2,3,4)))
  println(reverse(Nil))


  //EXERCISE 7
  /*
  Intersperse function: receives two parameters, an element which is going to be inserted into a list
  alternating by one position , and the second parameter is a list.
  IntersperseTemp function: receives the element and the list from intersperse, also receives other two parameters
  which are the new list with the element added, and a flag which is going to alternate his value between 1 and 2
  to execute the intersperseTemp function, which is called recursively in two scenarios, one with flag value of 1 and
  other with flag value of 2. This allows to change the element who is going to be the new head of the list,
  and in the flag value of 1
   */
  def intersperse [A] (elem:A, lst:List[A]):List[A] = {
    @tailrec
    def intersperseTemp [A] (elem:A, lst:List[A],lst1:List[A],flag:Int):List[A] = (lst,elem,flag) match {
      case (Nil,elem,1) => lst1
      case (Const(h,t),elem,1) => intersperseTemp(elem,lst,addEnd(h,lst1),1+1)
      case (Const(h,t),elem,2) => intersperseTemp(elem,t,addEnd(elem,lst1),2-1)
    }
    intersperseTemp(elem,lst,Nil,1)
  }
  println(intersperse(1,List(2,3,4,5)))
  println(intersperse("a", List("b","c","d")))


  //EXERCISE 8
  /*
    concat function: Receives a list with lists of any type inside, as parameter.
    return a list with the concatenation of the lists inside the list.
    concatTemp function: receives the original list and a new empty list as parameters.
    the recursive call of concatTemp receives the tail of the function which are the remainder lists,
    and in the second parameter calls the append function to insert the head which is a list, to the new list.
   */
  def concat [A] ( lst:List[List[A]] ) :List[A] = {
    @tailrec
    def concatTemp [A] ( lst:List[List[A]], lst1:List[A]) :List[A] = lst match {
      case Nil => lst1
      case Const(h,t) => concatTemp(t,append(lst1,h))
    }
    concatTemp(lst,Nil)
  }

  println(concat(List(List(1,2,3),List(4,5,6))))
  println(concat(List(List("a","b"),List("c","d","e"))))
  println(concat(List(List(1.0,2.0),Nil,List(3.0,4.0))))

  //append function
  def append [A] ( lst1:List[A], lst2:List[A]) : List[A] = (lst1,lst2) match {
    case (Nil,Nil) => Nil
    case (lst1,Nil) => lst1
    case (Nil,lst2) => lst2
    case ( Const(h,t), lst2) => Const(h,append(t, lst2))

  }

  //dropWhile function: Removes items from a specified list, until a condition is met.
  def dropWhile [A] (lst:List[A]) (f:A=>Boolean) : List[A] = lst match {
    //case Nil => Nil
    case Const(h,t) if f(h) => dropWhile(t)(f)
    case _ => lst
  }

  println("dropWhile result1 " + dropWhile(List(1,2,3,4,5,6))((y:Int) => y < 3 ))
  println("dropWhile result2 " + dropWhile(List(1,2,3,4,5,6))((y:Int) => y < 7 ))
  val funLista = dropWhile(List(1,2,3,4,5,6))(_)
  println(funLista((x:Int) => x <=1))


  /*
  reduce function is
  A generic function that is used to apply repetitive processes for different implementations
   */
  def reduce ( lst:List[Int], z:Int) (f:(Int,Int) =>Int):Int = lst match {
    case Nil => z
    case Const(h,t) => f(h,reduce(t,z)(f))
  }

  println(reduce(List(1,2,3,4),0)((x,y)=>x+y))
  println(reduce(List(1,2,3,4),1)((x,y)=>x*y))

  def sumReduce (lst:List[Int]) = reduce(lst,0)((x,y)=>x+y)
  def mulReduce (lst:List[Int]) = reduce(lst,1)((x,y)=>x*y)


  //foldRight function:
  /*
  A generic function that is used to apply repetitive processes for different implementations
   */
  def foldRight [A,B] (as: List[A], z:B)(f: (A,B) => B) : B = as match {
    case Nil => z
    case Const(h,t) => f(h,foldRight(t,z)(f))
  }

  /* sumFR(List(1,2,3))
  case Cons(h,t) => f(1, foldRight((2,3),0)(f))    ------- 1 + foldRight((2,3),0)(f)
  case Cons(h,t) => f(2, foldRight( (3), 0)(f))    ------- 2+ foldRight( (3), 0)(f)
  case Cons(h,t) => f(3, foldRight( Nil, 0)(f))   -------- 3 + foldRight( Nil, 0)(f)
  case Nil  => 0

  3+0 = 3
  2+3 = 5
  1+5 = 6
   */

  def sumFR  (lst:List[Int]) = foldRight(lst,0)((x,y)=>x+y)
  //Another way to write the function sumFR : def sumFR  (lst:List[Int]) = foldRight(lst,0)(_+_)

  def multFR (lst:List[Int]) = foldRight(lst,1)((x,y)=>x*y)
  //Another way to write the function sumFR : def multFR  (lst:List[Int]) = foldRight(lst,0)(_*_)



  def sumarUno (lst:List[Int]) =
    foldRight(lst,Nil:List[Int])((elem,lst)=> Const(elem+1,lst))

  println("sumar uno " + sumarUno(List(1,2,3,4,5)))

  //Exercise 13.
  println("EXERCISE 13 " + foldRight(List(9L,6L,7L), Nil:List[Long] ) (Const(_,_)) )


  //Exercise 14.
  def lengthFR [A] (lst:List[A]) = foldRight(lst,0)((x,y)=>1+y)
  println("lengthFR " + lengthFR(List('a','b','c')))


  //Exercise 15.
  def and2 (lst:List[Boolean]):Boolean = foldRight(lst,true)(_&&_)
  println("EXERCISE 15 " + and2(List(true,true,true,false)))
  println("EXERCISE 15 " + and2(List(true,true,true,true)))

  /*
  and2(List(true,false,true))
  case Cons(h,t) => f(true, foldRight((false,true),true)(f))  ------ true && foldRight((false,true),true)(f)
  case Cons(h,t) => f(false, foldRight((true),true)(f))  ------ false && foldRight((true),true)(f))
  case Cons(h,t) => f(true, foldRight((Nil)),true)(f))  ------- true && foldRight((Nil)),true)(f))
  case Nil => z ---true

  true && true = true
  false && true = false
  true && false = false
 */


  //Exercise 16.
  def takeWhile [A] (lst:List[A]) (p:A => Boolean) =
    foldRight(lst,Nil:List[A])((h,t)=> if (p(h)) Const(h,t) else Nil)

  println("EXERCISE 16 " + takeWhile(List(1,2,3,4,5,6))(_<5))
  /*
  takeWhile(List(1,2,3))(_<3))
    if (1<3) --- (1,2,3)
    Const(h,t) => f(1, foldRight((2,3),Nil)f(_<3))
    if (2<3) ----- (2,3)
    Const(h,t) => f(2, foldRight((2,3),Nil)f(_<3))
    if (3<3) --- Nil
   */




  //Exercise 17.
  def filter [A] ( lst:List[A] ) (p:A => Boolean): List[A] =
    foldRight(lst,Nil:List[A])((h,t)=> if (p(h)) Const(h,t) else t)

  println("EXERCISE 17 " + filter(List(1,2,3,4,5,6,7,8))(_%2==0))

  /*
  filter(List(1,2,3))(_%2==0)
    foldRight(List(1,2,3),Nil)((2,3))
      case Const(1,(2,3)) => f(1, folRight((2,3),Nil)(f)) --------
     foldRight ((2,3),Nil)(Const(2,(2,3))
        case Const(2,3) => f(2, foldRight(3,Nil)(f)----------
     foldRight ((3,Nil) (Nil)
        case Nil => Nil
   */

  //EXERCISE 18.
  def unzip2[A,B] ( lst:List[(A,B)]) : (List[A],List[B]) =
    foldRight(lst,(Nil,Nil):(List[A],List[B]))((h,t)=> (Const(h._1,t._1), Const(h._2,t._2)))

  println("EXERCISE 18 " + unzip2(List((1,"a"),(2,"b"),(3,"b"))))

  /*
   unzip2(List((1,"a"),(2,"b")))
      foldRight(((1,"a"),(2,"b")),(Nil,Nil))
        case Const(h,t) => f((1,"a"), foldRight((2,"b"),(Nil,Nil)(f)))
      foldRight((2,"b"), (Nil,Nil))
        case Const(h,t) => f((2,"b"), foldRight(Nil,(Nil,Nil))f()
      foldRight (Nil,(Nil,Nil))
        case Nil => (Nil,Nil)
   */




  //foldLeft function:
  /*
  A generic function that is used to apply repetitive processes for different implementations
   */

  def foldLeft [A,B] ( lst:List[A], z:B) (f:(B,A) => B):B = lst match {
    case Const(h,t) => foldLeft(t,f(z,h))(f)
    case Nil => z
  }
/*
  def foldRight [A,B] (as: List[A], z:B)(f: (A,B) => B) : B = as match {
    case Nil => z
    case Const(h,t) => f(h,foldRight(t,z)(f))
  }


 */
  def sumFL (lst:List[Int]) = foldLeft(lst,0)(_+_)
  def multFL (lst:List[Int]) = foldLeft(lst,1)(_*_)


  //EXERCISE 19.
  def lengthL [A](lst:List[A]) :Int = foldLeft(lst,0)((y,x)=>y+1)


  //Exercise 20.
  def andL(lst:List[Boolean]): Boolean = foldLeft(lst, true)(_&&_)
  println("Exercise 20 " + andL(Nil))
  println("Exercise 20 " +andL(List(true)))
  println("Exercise 20 " +andL(List(true,false)))
  println("Exercise 20 " +andL(List(true,true,true)))

  //Exercise 21. takeWhile


  //Exercise 22
  def filterL[A](lst:List[A])(p:A=>Boolean):List[A] =
    foldLeft(lst,Nil:List[A])((lst,e) => if (p(e)) addEnd(lst,e) else lst)


  //Exercise 23
  def unzipL [A,B] (lst:List[(A,B)]):(List[A],List[B]) =
    foldLeft(lst,(Nil,Nil):(List[A], List[B]))((lst,elem)=>(addEnd(lst._1,elem._1),addEnd(lst._2,elem._2)))


}

