/* Temat:  Zły typ zwracanej wartosci w lambdzie
 *         (zauważmy, że w lambdzie typ wykrywany jest automatycznie, więc to przypisanie się nie uda)
 * Result: Cannot assign (Int,String) -> Int? to (Int,String) -> Int
 */

fun main() : Unit {
    val foo: (Int, String) -> Int = { y: Int, w: String ->
        val x: Int? = y;
        x;
    };
}
