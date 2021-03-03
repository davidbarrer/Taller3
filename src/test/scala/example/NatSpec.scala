package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {

  //Exercise 9.
  "Calling the function addNat with the parameters addNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero)))" should
    "return Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
    Nat.addNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Cero)))))
  }

  //Exercise 9.
  "Calling the function addNat2 with the parameters addNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero)))" should
    "return Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
    Nat.addNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Cero)))))
  }

  //Exercise 10
  "Calling the function prodNat with the parameters prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero)))" should
    "return Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
    Nat.prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
  }

  //Exercise 10
  "Calling the function prodNat2 with the parameters prodNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero)))" should
    "return Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
    Nat.prodNat2(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
  }

}
