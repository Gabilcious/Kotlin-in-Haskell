/* Temat:  Operacja na złych zmiennych
 * Result: Cannot compare ... and ...
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    if (1 == true) { }
}