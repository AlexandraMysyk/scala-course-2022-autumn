package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import Homework._

object HomeworkSpecification extends Properties("Homework"):

  import arbitraries.{given Arbitrary[Int], given Arbitrary[Nat]}

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):

  property ("isZero")=forAll{(n: Zero)=>
    n.isZero()
  }
  property("addition")=forAll{(n:Nat)=>
    this+n==n
  }
  property("toInt")=forAll{(n:Zero)=>
    n.toInt==0
  }
  property("equals")=forAll{(n:Any)=>
    this.equals(n)==(if n==0 then true else false )
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):

  property("isZero")=forAll{()=>
    this.isZero()==false
  }
  property("predecessor")=forAll{()=>
    this.predecessor()==this
  }
  property("addition")=forAll { (n: Nat) =>
    (this + n).getClass==Nat
    (this+n) - n == this
  }
  property("subtraction")=forAll{(n:Nat)=>
    if(this>n) then ((this-n).getClass==Nat) && ((this-n)+n==this)
  }
  property("toInt")=forAll{
    (this.toInt()).getClass()==Int
  }
  property("equals")=forAll{(n:Nat)=>
    this.equals(n)==(this==n)
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  properties("addition")=forAll {
    Nat1 + Nat2 == Nat2 + Nat1
  }
  properties("subtraction")=forAll{
    if Nat1>Nat2 then (Nat1-Nat2).getClass==Nat
    else throw Exception("Not a natural number")
  }
  properties("equals")=forAll{
    Nat1.equals(Nat2)==(Nat1==Nat2)
  }
end NatSpecification
