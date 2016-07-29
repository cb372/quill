package io.getquill.dsl

import scala.language.higherKinds
import io.getquill.WrappedType
import scala.annotation.compileTimeOnly
import io.getquill.quotation.NonQuotedException
import scala.language.experimental.macros
import io.getquill.ast.ScalarLift
import io.getquill.util.Messages._

trait EncodingDsl {
  this: CoreDsl =>

  type PrepareRow
  type ResultRow

  type Decoder[T] = io.getquill.context.Decoder[ResultRow, T]

  trait Encoder[-T] {
    def expand(name: String, value: T): List[ScalarLift] =
      List(ScalarLift(name, value, this))
    def apply(index: Int, value: T, row: PrepareRow): PrepareRow
  }

  object Encoder {
    def apply[T](f: (Int, T, PrepareRow) => PrepareRow) =
      new Encoder[T] {
        override def apply(index: Int, value: T, row: PrepareRow) =
          f(index, value, row)
      }
  }

  /* ************************************************************************** */

  def lift[T](v: T): T = macro macroz.DslMacro.lift[T]

  @compileTimeOnly(NonQuotedException.message)
  def liftScalar[T](v: T)(implicit e: Encoder[T]): T = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftCaseClass[T](v: T): T = NonQuotedException()

  /* ************************************************************************** */

  def liftBatch[B[_], T](v: B[T]): Query[T] = macro macroz.DslMacro.liftBatch[T]

  //  @compileTimeOnly(NonQuotedException.message)
  def liftBatchScalar[T, B[_]](v: B[T])(implicit e: Encoder[T]): Query[T] = NonQuotedException()

  @compileTimeOnly(NonQuotedException.message)
  def liftBatchCaseClass[B[_], T](v: B[T]): Query[T] = NonQuotedException()

  /* ************************************************************************** */

  case class MappedEncoding[I, O](f: I => O)

  def mappedEncoding[I, O](f: I => O) = MappedEncoding(f)

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    new Decoder[O] {
      def apply(index: Int, row: ResultRow) =
        mapped.f(decoder(index, row))
    }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    new Encoder[I] {
      def apply(index: Int, value: I, row: PrepareRow) =
        encoder(index, mapped.f(value), row)
    }

  implicit def wrappedTypeDecoder[T <: WrappedType] =
    MappedEncoding[T, T#Type](_.value)

  implicit def traversableEncoder[T](implicit enc: Encoder[T]): Encoder[Traversable[T]] =
    new Encoder[Traversable[T]] {
      override def expand(name: String, value: Traversable[T]): List[ScalarLift] =
        value.map(ScalarLift(name, _, enc)).toList
      override def apply(index: Int, values: Traversable[T], row: PrepareRow) =
        // TODO Create tech debt issue
        fail("Bug: Can't encode traversable directly, it should have been expanded.")
    }
}
