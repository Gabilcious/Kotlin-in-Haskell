/* Temat:  Próba przypisania null do zmiennej nonnull
 * Result: Cannot assign null to String
 */

fun main() : Unit {
   var str_non_null: String = "mama";
   println(str_non_null);

   str_non_null = null;
}
