fun g(x: Int) : Int {
    return x*x*x*x;
}

fun main() : Unit {
    val arr_2: Array<Int> = Array( 5, {i: Int -> i*i;} );
    for (i in arr_2) {
        println (i);
    }
    println ("");

    val f: (Int) -> Int = {i: Int -> i*i*i;};
    val arr_3: Array<Int> = Array(10, f);
    for (i in [1 until 10]) {
        println(arr_3[i]);
    }
    println ("");

    val arr_4: Array<Int> = Array(3, {x: Int -> g(x);});
    for (i in arr_4) {
        println(i);
    }
    println ("");
}
