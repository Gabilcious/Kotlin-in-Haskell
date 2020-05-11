/* Temat:  Wywołanie funkcji ze złym argumentem
 * Result: Cannot assign ... to ...
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    val a: Int? = 10;
    print ( fib (a) );
}