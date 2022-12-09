package karazin.scala.users.group.week3

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat =
      Nat+1

    infix def + (that: Nat): Nat

    infix def - (that: Nat): Nat

    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(int: Int) =
      if int<=0 then throw new Exception("Not a natural number")
      else int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat =
      that

    infix def -(that: Nat): Nat =
      throw new Exception("This is not a natural number")

    // Optional task
    def toInt: Int =
      0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean =
      if obj==0 then true
      else false

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = this

    infix def +(that: Nat): Nat =
      def f(sum:Nat,n:Nat):Nat=
        if n==0 then sum
        else f(sum.successor,n.predecessor)
      f(this,that)

    infix def -(that: Nat): Nat =
      def f(dif: Nat, n: Nat): Nat =
        if n == 0 then dif
        else f(dif.predecessor, n.predecessor)
      if (that>this) then throw Exception("This is not a natural number")
      else f(this,that)

    // Optional task
    def toInt: Int =
      this


    override def equals(obj: Any): Boolean =
      if obj.getClass != Nat then false
      else obj == this


