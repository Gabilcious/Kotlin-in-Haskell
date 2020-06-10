/* Temat:  Index out of bound
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in print statemenet:
 *      in array arr:
 *      index 20 is out of bound
 */

fun main() : Unit {
    val arr: Array<Int> = [0..10];
    print ( arr[20] );
}