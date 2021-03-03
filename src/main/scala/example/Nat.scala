package example
//Impĺementación de los Nat. Donde el caso base se tiene el Cero y el caso recursivo es el sucesor de un natural

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat

object Nat  {

  //Exercie 9.
  /*
  addNat function receives two parameters of Nat type.
  the intResult val converts both parameters to Ints with fromNatToInt function and adds them together.
  finally the fromIntToNat function converts the intResult result to a Nat value type.
   */
  def addNat( nat1:Nat, nat2:Nat ): (Nat) = {
    val intResult = fromNatToInt(nat1) + fromNatToInt(nat2)
    fromIntToNat(intResult)
  }

  println(addNat(Cero,Suc(Cero)))
  println(addNat(Suc(Suc(Cero)),Suc(Cero)))
  println(addNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))

  def addNat2 ( nat1:Nat, nat2:Nat ) : (Nat) = ( nat1 ) match{
    case Cero => nat2
    case Suc(n) => addNat2(n,Suc(nat2))
  }

  println("RESULTADO " + addNat2(Suc(Suc(Cero)),Suc(Cero)))
  println("RESULTADO2 " + addNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))


  //  Exercise 10.
  /*
  addNat function receives two parameters of Nat type.
  the intResult val converts both parameters to Ints with fromNatToInt function and multiply them together.
  finally the fromIntToNat function converts the intResult result to a Nat value type.
   */
  def prodNat ( nat1:Nat , nat2:Nat ): (Nat) = {
    val intResult = fromNatToInt(nat1) * fromNatToInt(nat2)
    fromIntToNat(intResult)
  }

  println(prodNat(Cero,Suc(Cero)))
  println(prodNat(Suc(Suc(Cero)),Suc(Cero)))
  println(prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))


  def prodNat2 ( nat1:Nat, nat2:Nat  ) : Nat = {
    def prodNat2Temp ( nat1:Nat, nat2:Nat, nat3:Nat ) : Nat = ( nat1 ) match {
      case (Cero) => Cero
      case (Suc(Cero)) => nat3
      case (Suc(n)) => prodNat2Temp ( n , nat2 , addNat2(nat3,nat2) )
    }
    prodNat2Temp(nat1, nat2, nat2)
  }

  println("RESULTADO 3 " + prodNat2(Suc(Cero),Suc(Suc(Cero))))
  println("RESULTADO 4 " + prodNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))


  //función fromNatToInt que toma un número natural Nat y lo transforma a su valor Int.
  def fromNatToInt(nat:Nat):Int = nat match {
    case Cero => 0
    case Suc(n) => 1 + fromNatToInt(n)
  }
  // función fromIntToNat que toma un número entero Int y lo transforma a su valor correspondiente natural.
  def fromIntToNat(int:Int):Nat = int match {
    case 0 => Cero
    case n => Suc(fromIntToNat(n-1))
  }
}


//Cero, suc(cero), suc(suc(cero))