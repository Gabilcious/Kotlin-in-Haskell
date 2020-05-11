/* Temat:  Zamiana zmiennej const stworzonej w pętli
 * Result: Val cannot be reassigned
 */

fun main() : Unit {
    for (i in [1..10]) {
        i = 15;
    }
}
