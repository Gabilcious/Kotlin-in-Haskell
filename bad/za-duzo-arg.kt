/* Temat:  Za dużo argumentów przy wywowływaniu funkcji
 * Result: Passed 2 arguments, when Ident "fib" expects 1
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    print ( fib (10, 2) );
}