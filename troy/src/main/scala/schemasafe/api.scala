package schemasafe

import scala.reflect.macros.blackbox.Context
import schemasafe.utils.XResult
import singleton.ops._

class LiteralQueryWrapper[LQ <: XString] {
  def :+[LQ2 <: XString](q2: LQ2)(implicit concat: LQ + LQ2): LiteralQueryWrapper[concat.OutString] =
    new LiteralQueryWrapper[concat.OutString]

  def +:[LQ2 <: XString](rq: LQ2)(implicit concat: LQ2 + LQ): LiteralQueryWrapper[concat.OutString] =
    new LiteralQueryWrapper[concat.OutString]

  def ++[LQ2 <: XString](rq: LiteralQueryWrapper[LQ2])(implicit concat: LQ + LQ2): LiteralQueryWrapper[concat.OutString] =
    new LiteralQueryWrapper[concat.OutString]

  def apply[I, O]: LiteralQueryWithTypesWrapper[LQ, I, O] =
    new LiteralQueryWithTypesWrapper[LQ, I, O]

}
object LiteralQueryWrapper {
  def apply[LQ <: XString]: LiteralQueryWrapper[LQ] = new LiteralQueryWrapper[LQ] {}
}

class LiteralQueryWithTypesWrapper[LQ <: XString, I, O] {
  def materialize[R <: XResult[_], Success](
    implicit
    matOrError: TypedQueryMaterializer.Aux[LQ, I, O, R],
    getter: Get.Aux[R, Success]
  ): Success = getter(matOrError.result)
}


/**
  * @tparam LQ Literal String Query
  * @tparam I User case class representing Params or Input
  * @tparam O User case class representing Row or Output
  * @tparam S Maz size of the Result set (if known)
  * @tparam W Wire Format (Array[Bytes] or similar)
  * @tparam E Error type in case encoder failed (usually Exception or String)
  */
trait TypedQuery[LQ <: XString, I, O, S <: Option[XInt], W, E] {
  /**
    * String representation of LQ
    */
  def rawQuery: LQ

  /**
    * Maximum size of result set, as inferred from the query.
    * If None means the query didn't have limit clause.
    */
  def resultSetMaxSize: S

  /**
    * Encodes the user data types representing the query params to format suitable to be sent on wire (bytes probably)
    */
  def inputEncoder: I => W

  /**
    * Decodes the result set returned by executing the query to the user type representing the selected columns/fields.
    */
  def outputDecoder: W => Either[E, O]
}

/**
  * 1. Parse the query (via macro)
  * 2. Schema Check (find Schema types of Input & Output)
  * 3. Pair with Codecs
  */
trait TypedQueryMaterializer[LQ <: XString, I, O] {
  type W // Wire Format (Array[Bytes] or similar)
  type E // Error type in case encoder failed (usually Exception or String)
  type Result <: XResult[_]
  def result: Result
}
object TypedQueryMaterializer {
  type Aux[LQ <: XString, I, O, R <: XResult[_]] = TypedQueryMaterializer[LQ, I, O] {
    type Result = R
  }

  def instance[LQ <: XString, I, O, R <: XResult[TypedQuery[LQ, I, O, _, _, _]]](res: R): Aux[LQ, I, O, R] =
    new TypedQueryMaterializer[LQ, I, O] {
      override type Result = R
      override def result: Result = res
    }
}


object utils {
  type XResult[+T] = Either[XString, T]
}
trait Get[R <: XResult[_]] {
  type Out
  def apply(result: R): Out
}
object Get {
  type Aux[R <: XResult[_], O] = Get[R] { type Out = O }

  implicit def instance[E, O]: Aux[Right[Nothing, O], O] = new Get[Right[Nothing, O]] {
    override type Out = O
    override def apply(result: scala.Right[Nothing, Out]): Out = result.value
  }

  implicit def failedInstance[E <: XString]: Aux[Left[E, Nothing], Nothing] = macro fail[E]

  def fail[E <: XString](c: Context)(implicit eType: c.WeakTypeTag[E]) =
    c.abort(c.enclosingPosition, eType.tpe.toString)
}

object api {
  def q[Q <: XString](q: Q): LiteralQueryWrapper[Q] = LiteralQueryWrapper.apply[Q]
}
