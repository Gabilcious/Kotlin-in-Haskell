/* Temat:  Return w lambdzie
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in declaration foo:
 *      Return is not allowed inside lambda body
 */

fun main() : Unit {
    val foo: () -> Int = { x: Int ->
        return x;
    };
}
