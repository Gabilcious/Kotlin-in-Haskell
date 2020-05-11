/* Temat:  Lambda przekazywana w prametrze konstruktora Array zwraca zły typ
 * Result: Lambda should not return Unit
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, {x: Int ->});
}