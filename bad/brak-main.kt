/* Temat:  Odpalenie pliku bez main
 * Result:
 * interpreter: Error has occured:
 *      Main was not found
 */

fun fib(d: Int) : Int {
    return fib(d-1) + fib(d-2);
}
