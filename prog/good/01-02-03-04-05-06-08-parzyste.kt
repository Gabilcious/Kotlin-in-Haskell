fun main() : Unit {
    var i: Int = 0;
    
    while (i++ < 5) {
        val jest_parzyste: Bool = i % 2 == 0;
        var wyn: String = "Parzyste";
        if (jest_parzyste) {
            wyn = "Nieparzyste";
        }
        print (wyn + "\n");
    }

    println("");

    i = 0;
    while (i++ < 5) {
        println (i % 2 == 0 ? "Parzyste" : "Nieparzyste");
    }
}
