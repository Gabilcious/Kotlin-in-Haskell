/* Temat:  Lambda przekazywana w prametrze konstruktora Array przyjmuje zły typ
 * Result: Lambda should take only one Int as argument
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, {x: String -> 6;});
}