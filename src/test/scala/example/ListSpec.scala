package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {

  //Exercise 1.
  "Calling the function take with the parameters take(3,List('a','b','c','d','e')" should
   "return Const(1,Const(2,Const(3,Nil)))" in {
    List.take(3,List('a','b','c','d','e')) shouldEqual Const('a',Const('b',Const('c',Nil)))
  }


  //Exercie 2.
  "Calling the function init with the parameters init(List(1,2,3,4,5,6)" should
   "return Const(1,Const(2,Const(3,Const(4,Const(5,Nil)))" in {
  List.init(List(1,2,3,4,5,6)) shouldEqual Const(1,Const(2,Const(3,Const(4,Const(5,Nil)))))
  }


  //Exercise 3.
 "Calling the function split with the parameters split(3,List(1,2,3,4,5,6,7))" should
   "return (Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Const(7,Nil)))" in {
  List.split(3,List(1,2,3,4,5,6,7)) shouldEqual ((Const(1,Const(2,Const(3,Nil))),Const(4,Const(5,Const(6,Const(7,Nil))))))
 }

 //Exercise 4.
 "Calling the function zip with the parameters zip(List(1,2,3),List(true,false,true,true))" should
   "return Const((1,true),Const((2,false),Const((3,true),Nil))), Nil)))" in {
  List.zip(List(1,2,3),List(true,false,true,true)) shouldEqual Const((1,true),Const((2,false),Const((3,true),Nil)))
 }

 //Exercise 5.
  "Calling the function unzip with the parameters unzip(List((1,'a'),(2,'b'),(3,'b'))" should
    "return (Const(1,Const(2,Const(3,Nil))),Const(a,Const(b,Const(b,Nil))))" in {
    List.unzip(List((1,'a'),(2,'b'),(3,'b'))) shouldEqual (Const(1,Const(2,Const(3,Nil))),Const('a',Const('b',Const('b',Nil))))
  }

  //Exercise 6.
  "Calling the function reverse with the parameters reverse(List(1,2,3,4))" should
    "return Const(4,Const(3,Const(2,Const(1,Nil))))" in {
    List.reverse(List(1,2,3,4)) shouldEqual Const(4,Const(3,Const(2,Const(1,Nil))))
  }


  //Exercise 7.
  "Calling the function intersperse with the parameters intersperse(1,List(2,3,4,5))" should
    "return Const(2,Const(1,Const(3,Const(1,Const(4,Const(1,Const(5,Const(1,Nil))))))))" in {
    List.intersperse(1,List(2,3,4,5)) shouldEqual Const(2,Const(1,Const(3,Const(1,Const(4,Const(1,Const(5,Const(1,Nil))))))))
  }

  //Exercie 8.
  "Calling the function concat with the parameters concat(List(List(1,2,3),List(4,5,6)))" should
    "return Const(1,Const(2,Const(3,Const(4,Const(5,Const(6,Nil))))))" in {
    List.concat(List(List(1,2,3),List(4,5,6))) shouldEqual Const(1,Const(2,Const(3,Const(4,Const(5,Const(6,Nil))))))
  }



}







