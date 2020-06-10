/* Temat:  Dzielenie przez 0
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in while statemenet:
 *      in println statemenet:
 *      Cannot divide by 0
 */

fun main() : Unit {
    var i: Int = 5;
    while (i-- > 0) {
        println (1 / i);
    }
}
