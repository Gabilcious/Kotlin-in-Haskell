/* Temat:  Za dużo argumentów przy wywowływaniu funkcji
 * Result: Passed ... arguments, when Ident "..." expects ...
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    print ( fib (10, 2) );
}