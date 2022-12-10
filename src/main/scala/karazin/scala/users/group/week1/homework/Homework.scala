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

object Homework:

  object `Boolean Operators`:

def not(b: Boolean): Boolean =
  if b then false
  else true

def and(left: Boolean, right: Boolean): Boolean =
  if left then right
  else false

def or(left: Boolean, right: Boolean): Boolean =
  if not(left) then right
  else true

end `Boolean Operators`

object `Fermat Numbers`:

  val multiplication: (BigInt, BigInt) => BigInt =
    val multiplicationRec: (BigInt, BigInt, BigInt) => BigInt =
      (a, b, res) =>
        if b < 1 then res
        else multiplicationRec(a, b - 1, res + a)
    (a, b) =>
      if b > 0 then multiplicationRec(a, b, 0)
      else -1 * multiplicationRec(a, b.abs, 0)

  //assume that the exponent is a positive number
  val power: (BigInt, BigInt) => BigInt =
    val powerRec: (BigInt, BigInt, BigInt) => BigInt =
      (a, b, res) =>
        if b < 1 then res
        else powerRec(a, b, multiplication(a, res))
    (a, b) =>
      powerRec(a, b, 1)

  val fermatNumber: Int => BigInt =
    n =>
      power(2, power(2, n)) + 1

end `Fermat Numbers`

//didn't check
object `Look-and-say Sequence`:
  val lookAndSaySequenceElement: Int => BigInt =
    val countInner: (String, Int, Int, Int) => String =
      (str, i, k, j) =>
        if str(i) != str(i + 1) then str.appended(str(i), j)
        else countInner(str, i + 1, k, j + 1)
    val countOuter: (String, Int, Int) => Int =
      (str, i, k) =>
        if str(i) == 0 then return 1
        else countinner(str, i, k, 0).toInt
    val f: (Int, String, Int) => String =
      (n, str, counter) =>
        if counter == n then str
        else {
          countOuter(str, 0, 0)
          f(n, str, counter + 1)
        }
    n =>
      f(n, "1", 1);


end `Look-and-say Sequence`

object `Kolakoski Sequence`:
  val kolakoskiNumber: Int => Int =
    val count: (Array[Int], Int, Int, Int) => Array[Int] =
      (arr, counter, i, a) =>
        if i >= arr(counter) then arr.add(a)
        else count(arr, counter - 1, i, a)
    val f: (Int, Int, Array[Int]) => Int =
      (n, counter, arr[2 * n])
      =>
      if arr(n) == 1 or arr(n) == 2 then arr(n)
      else
        if counter % 2 == 1 then a = 1
        else a = 2
        count(arr, counter - 1, 0, a)
        f(n, counter + 1, arr);
    n =>
      f(n, 1, Array(1));
end `Kolakoski Sequence`

end Homework
