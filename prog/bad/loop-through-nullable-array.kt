/* Temat:  Próba przejścia forem przez Array, które może być nullerm
 * Result: Loop for can only go through nonull iterable elements
 */

fun main() : Unit {
    val arr: Array<Int>? = [0..10];
    for (i in arr) { }
}