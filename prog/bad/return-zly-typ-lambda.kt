/* Temat:  Zły typ zwracanej wartosci w lambdzie
 *         (zauważmy, że w lambdzie typ wykrywany jest automatycznie, więc to przypisanie się nie uda)
 * Result: Cannot assign ... to ...
 */

fun main() : Unit {
    val foo: () -> Int = { x: Int ->
        val y: Int? = x;
        y;
    };
}
