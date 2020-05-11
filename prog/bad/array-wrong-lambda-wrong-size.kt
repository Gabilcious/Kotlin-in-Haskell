/* Temat:  Kontruktor Array jest wywoływany bez poprawnego size
 * Result: Size of array should be Int
 */

fun main() : Unit {
    val arr: Array<Int> = Array(true, {x: Int -> 10;});
}