/* Temat:  Lambda przekazywana w prametrze konstruktora Array przyjmuje zly typ
 * Result: interpreter: Error has occured:
 *      in function main:
 *      in declaration arr:
 *      Lambda should take only one Int as argument
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, {x: String -> 6;});
}