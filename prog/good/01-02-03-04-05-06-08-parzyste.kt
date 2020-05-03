fun main() : Unit {
    var i: Int = 0;
    
    while (i++ < 10) {
        val jest_parzyste: Bool = i % 2 == 0;
        var wyn: String = "Parzyste";
        if (jest_parzyste) {
            wyn = "Nie parzyste";
        }
        print (wyn + "\n");
    }
}
