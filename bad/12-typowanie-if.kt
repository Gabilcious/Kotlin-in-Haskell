/* Temat:  Zly typ w if / assert
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      Wrong expresion inside if statement: (2+2) is not Bool but Int
 */

fun main() : Unit {
    if (2 + 2) { }
}
