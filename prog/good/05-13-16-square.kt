fun main() : Unit {
    val sqr: (Int) -> Int = {x: Int ->
        var i: Int = 1;
        while (++i <= x) {
            if (i*i >= x) {
                break;
            }
            else {
                continue;
            }
            println("Error?");
        }
        i;
    };
    
    assert(sqr(16) == 4);
}
