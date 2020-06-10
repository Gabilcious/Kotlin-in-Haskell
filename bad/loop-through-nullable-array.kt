/* Temat:  Proba przejscia forem przez Array, ktore może byc nullerm
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in for statement:
 *      Loop for can only go through nonnull Array
 */

fun main() : Unit {
    val arr: Array<Int>? = [0..10];
    for (i in arr) { }
}