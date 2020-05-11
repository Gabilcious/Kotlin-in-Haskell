/* Temat:  Return w lambdzie
 * Result: Return is not allowed inside lambda body
 */

fun main() : Unit {
    val foo: () -> Int = { x: Int ->
        return x;
    };
}
