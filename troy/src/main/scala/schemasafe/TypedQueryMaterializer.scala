package schemasafe

import schemasafe.utils.XResult
import shapeless.HList
import singleton.ops.{XInt, XString}

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


trait ConfigurationProvider {
  type W // Wire Format (Array[Bytes] or similar)
  type E // Error type in case encoder failed (usually Exception or String)
}

// Needs a macro
trait QueryParser[LQ <: XString] {
  /**
    * Typelevel ADT representation of LQ (Literal query)
    */
  type TQ <: XResult[_]
}
object QueryParser {
  type Aux[LQ <: XString, _TQ <: XResult[_]] = QueryParser[LQ] { type TQ = _TQ }
  def instance[LQ <: XString, _TQ <: XResult[_]]: Aux[LQ, _TQ] = new QueryParser[LQ] {
    override type TQ = _TQ
  }
}
// TODO: Merge QueryParser & QueryAnalyzer to allow implementations to skip this and analyze via a macro

// Needs Schema
trait QueryAnalyzer[TQ] {
  /**
    * Maximum size of result set, as inferred from the query.
    * If None means the query didn't have limit clause.
    */
  type S <: Option[XInt]

  /**
    * Typelevel representation of Input (parameters) using Database types
    */
  type TI <: HList

  /**
    * Typelevel representation of Output (Selected columns) using Database types
    */
  type TO <: HList
}
object QueryAnalyzer {
  type Aux[TQ, _S <: Option[XInt], _TI <: HList, _TO <: HList] = QueryAnalyzer[TQ] {
    type S = _S
    type TI = _TI
    type TO = _TO
  }

  def instance[TQ, _S <: Option[XInt], _TI <: HList, _TO <: HList]: Aux[TQ, _S, _TI, _TO] = new QueryAnalyzer[TQ] {
    override type S = _S
    override type TI = _TI
    override type TO = _TO
  }
}


trait QueryCodecFinder[E, W, TI <: HList, TO <: HList, I, O] {
  /**
    * Encodes the user data types representing the query params to format suitable to be sent on wire (bytes probably)
    */
  type IE <: XResult[I => W]
  def inputEncoder: IE

  /**
    * Decodes the result set returned by executing the query to the user type representing the selected columns/fields.
    */
  type OD <: XResult[W => Either[E, O]]
  def outputDecoder: OD
}