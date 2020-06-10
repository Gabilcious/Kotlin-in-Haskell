/* Temat:  Zamiana zmiennej const stworzonej w petli
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in for statement:
 *      in statement ((i=15)):
 *      Val cannot be reasigned
 */

fun main() : Unit {
    for (i in [1..10]) {
        i = 15;
    }
}
