/* Temat:  Operacja na zlych zmiennych
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      Cannot compare Int and Bool
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    if (1 == true) { }
}