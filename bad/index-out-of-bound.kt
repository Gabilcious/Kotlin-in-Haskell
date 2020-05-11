/* Temat:  Index out of bound
 * Result: Array Index Out of Bound
 */

fun main() : Unit {
    val arr: Array<Int> = [0..10];
    print ( arr[20] );
}