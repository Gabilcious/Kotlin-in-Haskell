/* Temat:  Zamiana zmiennej const
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in statement ((x="V2")):
 *      Val cannot be reasigned
 */

fun main() : Unit {
    val x: String = "V1";
    x = "V2";
}
