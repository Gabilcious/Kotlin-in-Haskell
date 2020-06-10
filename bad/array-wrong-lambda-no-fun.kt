/* Temat:  Zly konstruktor wolany dla Array
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in declaration arr:
 *      Second arg of Array constructor should be function
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, 10);
}