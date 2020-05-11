val a: Array<Array<Int>> = Array(
        10,
        { x: Int ->
            [x..x+5]; }
);

val b: Array<Array<Int?>> = Array(
        5,
        { x: Int ->
            Array(5, {y: Int ->
                                var ret : Int?;
                                if (x + y != 4) { ret = y; }
                                ret;
                            }
            );
        }
);

fun showTab(tab: Array<Array<Int?>>): Unit {
    for (x in tab) {
        for (j in [0 until 5]) {
            print (x[j]);
            print (" ");
            continue;
        }
        println ("");
    }
    println ("");
}

fun main() : Unit {
    showTab(a);
    showTab(b);
}