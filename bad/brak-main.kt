/* Temat:  Odpalenie pliku bez main
 * Result: Main was not found
 */

fun fib(d: Int) : Int {
    return fib(d-1) + fib(d-2);
}
