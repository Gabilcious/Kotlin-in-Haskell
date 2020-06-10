/* Temat:  Negative index
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in print statemenet:
 *      in array arr:
 *      index -3 is negative
 */

fun main() : Unit {
    val arr: Array<Int> = [0..10];
    print ( arr[-3] );
}