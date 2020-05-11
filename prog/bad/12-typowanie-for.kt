/* Temat:  Próba użycia złego typu w iterable
 * Result: Iterable should be <Integer>...<Integer>, not <...>..<...>
 */

fun main() : Unit {
    for (i in [true..false]) {
    }
}
