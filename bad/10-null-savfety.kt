/* Temat:  Proba przypisania null do zmiennej nonnull
 * Result:
 * interpreter: Error has occured:
 *      in function main:
 *      in statement ((str_non_null=null)):
 *      Got null when String is expected
 */

fun main() : Unit {
   var str_non_null: String = "mama";
   println(str_non_null);

   str_non_null = null;
}
