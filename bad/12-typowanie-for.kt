/* Temat:  Próba użycia złego typu w iterable
 * Result: Iterable should be <Int>..<Int>, not <Bool>..<Bool>
 */

fun main() : Unit {
    for (i in [true..false]) {
    }
}
