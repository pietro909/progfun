
/**
 * s = a | b & ~(a&b)
 * c = a & b
 **/

class Wire()

object circuit {


    val a, b, c = new Wire

    def inverter(input: Wire, output: Wire): Unit = ???
    def andGate(a1: Wire, a2: Wire, output: Wire): Unit = ???
    def orGate(o1: Wire, o2: Wire, output: Wire): Unit = ???

    def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
        val e = new Wire
        val i = new Wire
        andGate(a, b, c)
        orGate(a, b, e)
        inverter(c, i)
        andGate(e, i, s)
    }

    def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
        val s = new Wire
        val c1 = new Wire
        val c2 = new Wire
        halfAdder(b, cin, s, c1)
        halfAdder(a, s, sum, c2)
        orGate(c1, c2, cout)
    }

}
