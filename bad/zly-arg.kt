/* Temat:  Wywolanie funkcji ze zlym argumentem
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in print statement:
 *      Got Int? when Int is expected
 */

fun fib(d: Int) : Int {
    return fib (d - 1) + fib (d - 2);
}

fun main() : Unit {
    val a: Int? = 10;
    print ( fib (a) );
}