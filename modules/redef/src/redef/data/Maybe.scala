package redef.data

import scala.{CanEqual, CanThrow}
import scala.collection.{Iterable, IterableOnce, Iterator, List, Nil}
import scala.language.implicitConversions

case class Just[A](value: A) extends AnyVal

opaque type Maybe[+A] >: (Just[A] | None) = Just[A] | Nothing
