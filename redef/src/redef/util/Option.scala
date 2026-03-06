package redef.util

import scala.{CanEqual, CanThrow}
import scala.collection.{Iterable, IterableOnce, Iterator, List, Nil}
import scala.language.implicitConversions

case class Some[A](value: A) extends AnyVal

opaque type None = Null

final val None: None = null

opaque type Option[+A] >: (Some[A] | None) = Some[A] | None

extension [A](self: Option[A])

  /**
   * Returns true if the option is $none, false otherwise.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(_) => false
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
   *  case Some(_) => true
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
   *  case Some(x) => x
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
      case Some(x) => x
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
   *  case Some(x) => x
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
   *  case Some(x) => x
   *  case None    => null
   * }
   * ```
   *
   * @example
   *
   * ```
   * val initialText: Option[String] = getInitialText
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
   *  case Some(x) => Some(f(x))
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
  final inline def map[B](f: A => B): Option[B] =
    if (isEmpty) None else Some(f(get))

  /**
   * Returns the result of applying $f to this $option's value if the $option is
   * nonempty. Otherwise, evaluates expression `ifEmpty`.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(x) => f(x)
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
   *  case Some(x) => f(x)
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
  final inline def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) None else f(get)

  /**
   * Returns the nested $option value if it is nonempty. Otherwise, return
   * $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(Some(b)) => Some(b)
   *  case _             => None
   * }
   * ```
   *
   * @example
   *   ```
   *   Some(Some("something")).flatten
   *   ```
   *
   * @param ev
   *   an implicit conversion that asserts that the value is also an $option.
   * @see
   *   flatMap
   */
  def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    if (isEmpty) None else ev(get)

  /**
   * Returns this $option if it is nonempty **and** applying the predicate $p to
   * this $option's value returns true. Otherwise, return $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(x) if p(x) => Some(x)
   *  case _               => None
   * }
   * ```
   *
   * @param p
   *   the predicate used for testing.
   */
  final inline def filter(p: A => Boolean): Option[A] =
    if (isEmpty || p(get)) self else None

  /**
   * Returns this $option if it is nonempty **and** applying the predicate $p to
   * this $option's value returns false. Otherwise, return $none.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(x) if !p(x) => Some(x)
   *  case _                => None
   * }
   * ```
   *
   * @param p
   *   the predicate used for testing.
   */
  final inline def filterNot(p: A => Boolean): Option[A] =
    if (isEmpty || !p(get)) self else None

  /**
   * Returns false if the option is $none, true otherwise.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(_) => true
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
   *  case Some(x) => x == elem
   *  case None    => false
   * }
   * ```
   *
   * @example
   *   ```
   *   // Returns true because Some instance contains string "something" which equals "something".
   *   Some("something") contains "something"
   *
   *   // Returns false because "something" != "anything".
   *   Some("something") contains "anything"
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
   *  case Some(x) => p(x)
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
   *  case Some(x) => p(x)
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
   *  case Some(x) => f(x)
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
   *   // Returns Some(HTTP) because the partial function covers the case.
   *   Some("http") collect {case "http" => "HTTP"}
   *
   *   // Returns None because the partial function doesn't cover the case.
   *   Some("ftp") collect {case "http" => "HTTP"}
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
  final inline def collect[B](pf: PartialFunction[A, B]): Option[B] =
    if (!isEmpty) pf.lift(self.get) else None

  /**
   * Returns this $option if it is nonempty, otherwise return the result of
   * evaluating `alternative`.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *  case Some(x) => Some(x)
   *  case None    => alternative
   * }
   * ```
   *
   * @param alternative
   *   the alternative expression.
   */
  final inline def orElse[B >: A](alternative: => Option[B]): Option[B] =
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
   *   case (Some(x), Some(y)) => Some((x, y))
   *   case _                  => None
   * }
   * ```
   * @example
   *   ```
   *   // Returns Some(("foo", "bar")) because both options are nonempty.
   *   Some("foo") zip Some("bar")
   *
   *   // Returns None because `that` option is empty.
   *   Some("foo") zip None
   *
   *   // Returns None because `this` option is empty.
   *   None zip Some("bar")
   *   ```
   *
   * @param that
   *   the options which is going to be zipped
   */
  final def zip[A1 >: A, B](that: Option[B]): Option[(A1, B)] =
    if (isEmpty || that.isEmpty) None else Some((self.get, that.get))

  /**
   * Converts an Option of a pair into an Option of the first element and an
   * Option of the second element.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *   case Some((x, y)) => (Some(x), Some(y))
   *   case _            => (None,    None)
   * }
   * ```
   *
   * @tparam A1
   *   the type of the first half of the element pair
   * @tparam A2
   *   the type of the second half of the element pair
   * @param asPair
   *   an implicit conversion which asserts that the element type of this Option
   *   is a pair.
   * @return
   *   a pair of Options, containing, respectively, the first and second half of
   *   the element pair of this Option.
   */
  final def unzip[A1, A2](using
      asPair: A <:< (A1, A2)
  ): (Option[A1], Option[A2]) =
    if isEmpty
    then (None, None)
    else {
      val e = asPair(self.get)
      (Some(e._1), Some(e._2))
    }

  /**
   * Converts an Option of a triple into three Options, one containing the
   * element from each position of the triple.
   *
   * This is equivalent to:
   *
   * ```
   * option match {
   *   case Some((x, y, z)) => (Some(x), Some(y), Some(z))
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
   *   an implicit conversion which asserts that the element type of this Option
   *   is a triple.
   * @return
   *   a triple of Options, containing, respectively, the first, second, and
   *   third elements from the element triple of this Option.
   */
  final def unzip3[A1, A2, A3](using
      asTriple: A <:< (A1, A2, A3)
  ): (Option[A1], Option[A2], Option[A3]) = {
    if (isEmpty)
      (None, None, None)
    else {
      val e = asTriple(self.get)
      (Some(e._1), Some(e._2), Some(e._3))
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
   *  case Some(x) => List(x)
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

inline given (using inline ce: CanEqual[A, B]): CanEqual[Option[A], Option[B]] =
  CanEqual.derived

given IterableOnce[A] with

  def iterator: Iterator[A] =
    if isEmpty then Iterator.empty else Iterator.single(get)

  override def knownSize: Int = if isEmpty then 0 else 1

end given

object Option:

  /** An implicit conversion that converts an option to an iterable value. */
  implicit def option2Iterable[A](xo: Option[A]): Iterable[A] =
    if (xo.isEmpty) Iterable.empty else Iterable.single(xo.get)

  /**
   * An Option factory which creates `Some(x)` if the argument is not null, and
   * None if it is null.
   *
   * @param x
   *   the value
   * @return
   *   Some(value) if value != null, None if value == null
   */
  inline def apply[A](x: A | Null): Option[A] = if (x == null) None else Some(x)

  /**
   * An Option factory which returns `None` in a manner consistent with the
   * collections hierarchy.
   */
  inline def empty[A]: Option[A] = None

  /**
   * When a given condition is true, evaluates the `a` argument and returns
   * `Some(a)`. When the condition is false, `a` is not evaluated and `None` is
   * returned.
   */
  inline def when[A](cond: Boolean)(a: => A): Option[A] =
    if (cond) Some(a) else None

  /**
   * Unless a given condition is true, this will evaluate the `a` argument and
   * return `Some(a)`. Otherwise, `a` is not evaluated and `None` is returned.
   */
  inline def unless[A](cond: Boolean)(a: => A): Option[A] =
    when(!cond)(a)

end Option
