package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

def not(b: Boolean): Boolean =
  if b then false
  else true

def and(left: Boolean, right: Boolean): Boolean =
  if not(left) then false
  else if not(right) then false
  else true

def or(left: Boolean, right: Boolean): Boolean =
  if left then true
  else if right then true
  else false

end `Boolean Operators`

object `Fermat Numbers` :

  val multiplication: (BigInt, BigInt) => BigInt =
    val multiplicationRec:(BigInt,BigInt,BigInt) =>BigInt=
      (a,b,res)=>
        if b<1 then res
        else multiplicationRec(a,b-1,res+a)
    (a, b)=>
      if b > 0 then multiplicationRec(a,b,0)
      else -1*multiplicationRec(a,b.abs,0)

  //assume that the exponent is a positive number
  val power: (BigInt, BigInt) => BigInt =
    val powerRec:(BigInt,BigInt,BigInt)=> BigInt=
      (a,b,res)=>
        if b<1 then res
        else powerRec(a,b,multiplication(a,res))
    (a,b)=>
      powerRec(a,b,1)

  val fermatNumber: Int => BigInt =
    n=>
      power(2,power(2,n))+1

end `Fermat Numbers`

//extremely unsure about this function
//added recursion instead of var
object `Look-and-say Sequence` :
  val lookAndSaySequenceElement: Int => Array[Int] =
    val countInner:(Array[Int],Int,Int,Int)=>Array[Int]=
      (acc,i,k,j)=>
        if acc(i)!=acc(i+1) then acc.appended[k,k+1](acc(i),j)
        else countInner(acc,i+1,k,j+1)
    val countOuter:(Array[Int],Int,Int)=>Int=
      (acc,i,k)=>
        if(acc(i)==0) then return 1
        else countinner(acc,i,k,0)
    val f:(Int,Array[Int],Int)=>Array[Int]=
      (n,acc,counter)=>
        if counter==n then acc
        else{
          countOuter(acc,0,0);
          f(n,newAcc,counter+1);
        }
    n=>
      f(n,Array(1),1);



end `Look-and-say Sequence`

object `Kolakoski Sequence`:
  val kolakoskiNumber: Int => Int=
    val count:(Array[Int],Int,Int,Int)=>Array[Int]=
    (arr,counter,i,a)=>
      if i>=arr(counter) then arr.add(a)
      else count(arr,counter-1,i,a)
    val f :(Int,Int,Array[Int])=>Int=
      (n,counter,arr[2*n])=>
      if arr(n) == 1 or arr(n)==2 then arr(n)
      else
        if counter%2==1 then a=1
        else a=2
        count(arr,counter-1,0,a);
        f(n,counter+1,arr);
    n=>
      f(n,1,Array(1));
end `Kolakoski Sequence`

end Homework
