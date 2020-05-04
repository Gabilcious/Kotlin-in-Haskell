fun g(x: Int) : Int {
    return x*x*x*x;
}

fun main() : Unit {
    val arr_2 = Array( 5, {i: Int -> i*i;} );
    for (i in arr_2) {
        println (i);
    }

    val f: (Int) -> Int = {i: Int -> i*i*i;};
    val arr_3 = Array(10, f);
    for (i in 1 until 10) {
        println(arr_3[i]);
    }

    val arr_4 = Array(3, {x: Int -> g(x);});
    for (i in arr_4) {
        println(i);
    }
}
