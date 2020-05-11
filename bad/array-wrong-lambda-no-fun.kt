/* Temat:  Zły kontruktor wołany dla Array
 * Result: Second arg of Array constructor should be function
 */

fun main() : Unit {
    val arr: Array<Int> = Array(10, 10);
}