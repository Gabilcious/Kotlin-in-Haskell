/* Temat:  Rozne typy zwracanej wartosci w ternary op
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in statement (((10<20) ? true : "false")):
 *      Return expressions have different types: Bool and String
 */

fun main() : Unit {
    10 < 20 ? true : "false";
}
