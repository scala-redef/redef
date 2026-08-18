package redef.data

import scala.{CanEqual, CanThrow}
import scala.collection.{Iterable, IterableOnce, Iterator, List, Nil}
import scala.language.implicitConversions

case class Present[A](value: A) extends AnyVal

opaque type Optional[+A] >: (Present[A] | Null) = Present[A] | Null

extension [A](self: Optional[A])

  /**
   * Returns true if the option is $none, false otherwise.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(_) => false
   *  case None    => true
   * }
   * ```
   */
  final def isEmpty: Boolean =
    self eq None

  /**
   * Returns true if the option is an instance of $some, false otherwise.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(_) => true
   *  case None    => false
   * }
   * ```
   */
  final def isDefined: Boolean =
    !isEmpty

  /**
   * Returns the option's value.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => x
   *  case None    => throw new Exception
   * }
   * ```
   *
   * @note
   *   The option must be nonempty.
   * @throws NoSuchElementException
   *   if the option is empty.
   */
  def get: A throws NoSuchElementException =
    self match {
      case Present(x) => x
      case None    => throw new NoSuchElementException("None.get")
    }

  /**
   * Returns the option's value if the option is nonempty, otherwise return the
   * result of evaluating `default`.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => x
   *  case None    => default
   * }
   * ```
   *
   * @param default
   *   the default expression.
   */
  final inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else get

  /**
   * Returns the option's value if it is nonempty, or `null` if it is empty.
   *
   * Although the use of null is discouraged, code written to use $option must
   * often interface with code that expects and returns nulls.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => x
   *  case None    => null
   * }
   * ```
   *
   * @example
   *
   * ```
   * val initialText: Optional[String] = getInitialText
   * val textField = new JComponent(initialText.orNull,20)
   * ```
   */
  final inline def orNull[A1 >: A](implicit ev: Null <:< A1): A1 =
    self.getOrElse(ev(null))

  /**
   * Returns a $some containing the result of applying $f to this $option's
   * value if this $option is nonempty. Otherwise return $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => Present(f(x))
   *  case None    => None
   * }
   * ```
   *
   * @note
   *   This is similar to `flatMap` except here, $f does not need to wrap its
   *   result in an $option.
   *
   * @param f
   *   the function to apply
   * @see
   *   flatMap
   * @see
   *   foreach
   */
  final inline def map[B](f: A => B): Optional[B] =
    if (isEmpty) None else Present(f(get))

  /**
   * Returns the result of applying $f to this $option's value if the $option is
   * nonempty. Otherwise, evaluates expression `ifEmpty`.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => f(x)
   *  case None    => ifEmpty
   * }
   * ```
   *
   * This is also equivalent to:
   *
   * ```
   * option.map(f).getOrElse(ifEmpty)
   * ```
   *
   * @param ifEmpty
   *   the expression to evaluate if empty.
   * @param f
   *   the function to apply if nonempty.
   */
  final inline def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(self.get)

  /**
   * Returns the result of applying $f to this $option's value if this $option
   * is nonempty. Returns $none if this $option is empty. Slightly different
   * from `map` in that $f is expected to return an $option (which could be
   * $none).
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => f(x)
   *  case None    => None
   * }
   * ```
   *
   * @param f
   *   the function to apply
   * @see
   *   map
   * @see
   *   foreach
   */
  final inline def flatMap[B](f: A => Optional[B]): Optional[B] =
    if (isEmpty) None else f(get)

  /**
   * Returns the nested $option value if it is nonempty. Otherwise, return
   * $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(Present(b)) => Present(b)
   *  case _             => None
   * }
   * ```
   *
   * @example
   *   ```
   *   Present(Present("something")).flatten
   *   ```
   *
   * @param ev
   *   an implicit conversion that asserts that the value is also an $option.
   * @see
   *   flatMap
   */
  def flatten[B](implicit ev: A <:< Optional[B]): Optional[B] =
    if (isEmpty) None else ev(get)

  /**
   * Returns this $option if it is nonempty **and** applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) if p(x) => Present(x)
   *  case _               => None
   * }
   * ```
   *
   * @param p
   *   the predicate used for testing.
   */
  final inline def filter(p: A => Boolean): Optional[A] =
    if (isEmpty || p(get)) self else None

  /**
   * Returns this $option if it is nonempty **and** applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) if !p(x) => Present(x)
   *  case _                => None
   * }
   * ```
   *
   * @param p
   *   the predicate used for testing.
   */
  final inline def filterNot(p: A => Boolean): Optional[A] =
    if (isEmpty || !p(get)) self else None

  /**
   * Returns false if the option is $none, true otherwise.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(_) => true
   *  case None    => false
   * }
   * ```
   *
   * @note
   *   Implemented here to avoid the implicit conversion to Iterable.
   */
  final def nonEmpty: Boolean =
    isDefined

  /**
   * Tests whether the option contains a given value as an element.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => x == elem
   *  case None    => false
   * }
   * ```
   *
   * @example
   *   ```
   *   // Returns true because Present instance contains string "something" which equals "something".
   *   Present("something") contains "something"
   *
   *   // Returns false because "something" != "anything".
   *   Present("something") contains "anything"
   *
   *   // Returns false when method called on None.
   *   None contains "anything"
   *   ```
   *
   * @param elem
   *   the element to test.
   * @return
   *   `true` if the option has an element that is equal (as determined by `==`)
   *   to `elem`, `false` otherwise.
   */
  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && self.get == elem

  /**
   * Returns true if this option is nonempty **and** the predicate $p returns
   * true when applied to this $option's value. Otherwise, returns false.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => p(x)
   *  case None    => false
   * }
   * ```
   *
   * @param p
   *   the predicate to test
   */
  final inline def exists(p: A => Boolean): Boolean =
    !isEmpty && p(self.get)

  /**
   * Returns true if this option is empty **or** the predicate $p returns true
   * when applied to this $option's value.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => p(x)
   *  case None    => true
   * }
   * ```
   *
   * @param p
   *   the predicate to test
   */
  final inline def forall(p: A => Boolean): Boolean = isEmpty || p(self.get)

  /**
   * Applies the given procedure $f to the option's value, if it is nonempty.
   * Otherwise, do nothing.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => f(x)
   *  case None    => ()
   * }
   * ```
   *
   * @param f
   *   the procedure to apply.
   * @see
   *   map
   * @see
   *   flatMap
   */
  final inline def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(self.get)
  }

  /**
   * Returns a $some containing the result of applying `pf` to this $option's
   * contained value, **if** this option is nonempty **and** `pf` is defined for
   * that value. Returns $none otherwise.
   *
   * @example
   *   ```
   *   // Returns Present(HTTP) because the partial function covers the case.
   *   Present("http") collect {case "http" => "HTTP"}
   *
   *   // Returns None because the partial function doesn't cover the case.
   *   Present("ftp") collect {case "http" => "HTTP"}
   *
   *   // Returns None because the option is empty. There is no value to pass to the partial function.
   *   None collect {case value => value}
   *   ```
   *
   * @param pf
   *   the partial function.
   * @return
   *   the result of applying `pf` to this $option's value (if possible), or
   *   $none.
   */
  final inline def collect[B](pf: PartialFunction[A, B]): Optional[B] =
    if (!isEmpty) pf.lift(self.get) else None

  /**
   * Returns this $option if it is nonempty, otherwise return the result of
   * evaluating `alternative`.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => Present(x)
   *  case None    => alternative
   * }
   * ```
   *
   * @param alternative
   *   the alternative expression.
   */
  final inline def orElse[B >: A](alternative: => Optional[B]): Optional[B] =
    if (isEmpty) alternative else self

  /**
   * Returns a $some formed from this option and another option by combining the
   * corresponding elements in a pair. If either of the two options is empty,
   * $none is returned.
   *
   * This is equivalent to:
   *
   * ```
   * (option1, option2) match {
   *   case (Present(x), Present(y)) => Present((x, y))
   *   case _                  => None
   * }
   * ```
   * @example
   *   ```
   *   // Returns Present(("foo", "bar")) because both options are nonempty.
   *   Present("foo") zip Present("bar")
   *
   *   // Returns None because `that` option is empty.
   *   Present("foo") zip None
   *
   *   // Returns None because `this` option is empty.
   *   None zip Present("bar")
   *   ```
   *
   * @param that
   *   the options which is going to be zipped
   */
  final def zip[A1 >: A, B](that: Optional[B]): Optional[(A1, B)] =
    if (isEmpty || that.isEmpty) None else Present((self.get, that.get))

  /**
   * Converts an Optional of a pair into an Optional of the first element and an
   * Optional of the second element.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *   case Present((x, y)) => (Present(x), Present(y))
   *   case _            => (None,    None)
   * }
   * ```
   *
   * @tparam A1
   *   the type of the first half of the element pair
   * @tparam A2
   *   the type of the second half of the element pair
   * @param asPair
   *   an implicit conversion which asserts that the element type of this Optional
   *   is a pair.
   * @return
   *   a pair of Options, containing, respectively, the first and second half of
   *   the element pair of this Optional.
   */
  final def unzip[A1, A2](using
      asPair: A <:< (A1, A2)
  ): (Optional[A1], Optional[A2]) =
    if isEmpty
    then (None, None)
    else {
      val e = asPair(self.get)
      (Present(e._1), Present(e._2))
    }

  /**
   * Converts an Optional of a triple into three Options, one containing the
   * element from each position of the triple.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *   case Present((x, y, z)) => (Present(x), Present(y), Present(z))
   *   case _               => (None,    None,    None)
   * }
   * ```
   *
   * @tparam A1
   *   the type of the first of three elements in the triple
   * @tparam A2
   *   the type of the second of three elements in the triple
   * @tparam A3
   *   the type of the third of three elements in the triple
   * @param asTriple
   *   an implicit conversion which asserts that the element type of this Optional
   *   is a triple.
   * @return
   *   a triple of Options, containing, respectively, the first, second, and
   *   third elements from the element triple of this Optional.
   */
  final def unzip3[A1, A2, A3](using
      asTriple: A <:< (A1, A2, A3)
  ): (Optional[A1], Optional[A2], Optional[A3]) = {
    if (isEmpty)
      (None, None, None)
    else {
      val e = asTriple(self.get)
      (Present(e._1), Present(e._2), Present(e._3))
    }
  }

  /**
   * Returns a singleton list containing the $option's value if it is nonempty,
   * or the empty list if the $option is empty.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Present(x) => List(x)
   *  case None    => Nil
   * }
   * ```
   */
  def toList: List[A] =
    if (isEmpty) List() else new ::(self.get, Nil)

  def toResult: Result[A, Unit] =
    if isEmpty
    then Failure(new NoSuchElementException("None.get"))
    else Ok(self.get)

  def toTry: Try[A] =
    if isEmpty
    then Failure(new NoSuchElementException("None.get"))
    else Ok(self.get)

inline given (using inline ce: CanEqual[A, B]): CanEqual[Optional[A], Optional[B]] =
  CanEqual.derived

given IterableOnce[A] with

  def iterator: Iterator[A] =
    if isEmpty then Iterator.empty else Iterator.single(get)

  override def knownSize: Int = if isEmpty then 0 else 1

end given

object Optional:

  /** An implicit conversion that converts an option to an iterable value. */
  implicit def option2Iterable[A](xo: Optional[A]): Iterable[A] =
    if (xo.isEmpty) Iterable.empty else Iterable.single(xo.get)

  /**
   * An Optional factory which creates `Present(x)` if the argument is not null, and
   * None if it is null.
   *
   * @param x
   *   the value
   * @return
   *   Present(value) if value != null, None if value == null
   */
  inline def apply[A](x: A | Null): Optional[A] = if (x == null) None else Present(x)

  /**
   * An Optional factory which returns `None` in a manner consistent with the
   * collections hierarchy.
   */
  inline def empty[A]: Optional[A] = None

  /**
   * When a given condition is true, evaluates the `a` argument and returns
   * `Present(a)`. When the condition is false, `a` is not evaluated and `None` is
   * returned.
   */
  inline def when[A](cond: Boolean)(a: => A): Optional[A] =
    if (cond) Present(a) else None

  /**
   * Unless a given condition is true, this will evaluate the `a` argument and
   * return `Present(a)`. Otherwise, `a` is not evaluated and `None` is returned.
   */
  inline def unless[A](cond: Boolean)(a: => A): Optional[A] =
    when(!cond)(a)

end Optional
