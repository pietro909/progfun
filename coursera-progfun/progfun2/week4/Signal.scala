

class Signal[T](expr: => T) {
    import Signal._
    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    protected def update(expr: => T): Unit = {
        myExpr = () => expr
        computeValue()
    }

    protected def computeValue(): Unit = {
        newValue = caller.withValue(this)(myExpr())
        if (myValue != newValue) {
            myValue = newValue
            val obs = observers
            observers = Set()
            obs.foreach(_.computeValue())
        }
    }

    def apply(): T = {
        observers += caller.value
        assert(!caller.value.observers.contains(this), "cyclic signal definition")
        myValue
    }
}

object NoSignal extends Signal[Nothing](???) {
    override def computeValue() = ()
}

object Signal {
    // DynamicVariable is thread safe
    private val caller = new StackableVariable[Signal[_]](NoSignal)
    def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extendes Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
}

object Var {
    def apply(expr: => T) = new Var(expr)
}

class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)
    def value = values.head
    def withValue[R](newValue: T)(op: => R): R = {
        values = newValue :: values
        try op finally values = values.tail
    }
}
