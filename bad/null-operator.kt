/* Temat:  Uzycie nonnull operatora na zmiennej, ktora jest nullem.
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in statement (f(y!!)):
 *      in function f call:
 *      Null pointer exeption
 */

fun f(x: Int) : Unit {}

fun main() : Unit {
    var y: Int?;

    f(y!!);     // NullPointerException
}
