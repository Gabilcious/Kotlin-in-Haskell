/* Temat:  Użycie nonnull operatora na zmiennej, która jest nullem.
 * Result: Null pointer exeption
 */

fun f(x: Int) : Unit {}

fun main() : Unit {
    var y: Int?;

    f(y!!);     // NullPointerException
}
