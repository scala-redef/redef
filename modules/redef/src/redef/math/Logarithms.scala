object Logarithms {
  opaque type Logarithm = Double
  object Logarithm {
    // These are the two ways to lift to the Logarithm type
    def apply(d: Double): Logarithm = math.log(d)
    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None
  }

  // Extension methods define opaque types' public APIs
  extension logarithmOps on(x: Logarithm) {
    def toDouble: Double = math.exp(x)
    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm): Logarithm = x + y
  }
}
