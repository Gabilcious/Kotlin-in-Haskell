/* Temat:  Za duzo argumentow przy wywowlywaniu funkcji
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in print statement:
 *      Passed 2 arguments, when fib expects 1
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    print ( fib (10, 2) );
}