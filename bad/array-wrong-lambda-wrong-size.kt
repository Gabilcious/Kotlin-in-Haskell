/* Temat:  Kontruktor Array jest wywolywany bez poprawnego size
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in declaration arr:
 *      Size of array should be Int
 */

fun main() : Unit {
    val arr: Array<Int> = Array(true, {x: Int -> 10;});
}