/* Temat:  Lambda przekazywana w prametrze konstruktora Array zwraca zly typ
 * Result: interpreter: Error has occured:
 *      in function main:
 *      in declaration arr:
 *      Lambda should not return Unit
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, {x: Int ->});
}