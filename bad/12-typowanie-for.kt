/* Temat:  Proba użycia zlego typu w iterable
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in for statement:
 *      Iterable shlould be <Int>..<Int>, not <Bool>..<Bool>
 */

fun main() : Unit {
    for (i in [true..false]) {
    }
}
