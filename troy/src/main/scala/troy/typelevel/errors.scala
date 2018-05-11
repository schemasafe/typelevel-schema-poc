/*
 * Copyright 2016 Tamer AbdulRadi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package troy.typelevel

import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

import shapeless._
import singleton.ops._

object Error {
  type Msg[E] = SafeString[E]

  type TableDoesNotExist[T <: XString] =
    Msg["Table " + T + " does not exist."]

  type ColumnDoesNotExist[T <: XString, C <: XString] =
    Msg["Column " + C + " does not exist in table " + T]

  type NativeColumnDoesNotSupportContainsOperator[T <: XString, C <: XString] =
    Msg["Column " + C + " in table " + T + " has native type, that does not support contains operator."]

  type CollectionColumnDoesNotSupportEqualsOperator[T <: XString, C <: XString] =
    Msg["Column " + C + " in table " + T + " has collection type, that does not support == operator."]
}
sealed trait Result[+E, +T]
sealed trait Success[+T] extends Result[Nothing, T]
sealed trait Failure[E <: Error.Msg[_]] extends Result[E, Nothing]

trait GetOrElseFail[R] {
  type Out
}
object GetOrElseFail {
  @implicitNotFound("Bug alert: implicit not found GetOrElseFail.Aux[${R}, ${O}]")
  type Aux[R, O] = GetOrElseFail[R] { type Out = O }

  def apply[R](implicit instance: GetOrElseFail[R]): Aux[R, instance.Out] = instance

  def instance[R, O]: Aux[R, O] = new GetOrElseFail[R] { override type Out = O }

  implicit def getEitherIfRight[T] = instance[Success[T], T]
  implicit def failEitherIfLeft[E <: Error.Msg[_]]: Aux[Failure[E], Nothing] = macro GetOrElseFailMacro.fail[E]
}

 object GetOrElseFailMacro {
  def fail[E <: Error.Msg[_]](c: Context)(implicit eType: c.WeakTypeTag[E]) = {
    import c.universe._
    c.inferImplicitValue(eType.tpe.dealias.typeArgs.head) match {
      case q"{ $klass;$_ }" =>
        klass.children.head.children.collect {
          case q"type OutString = $value" =>
            val StringPattern(errorMsg) = value.toString
            c.abort(c.enclosingPosition, errorMsg)
        }
      case _ => // fallthrough
    }
    c.abort(c.enclosingPosition, s"Troy bug alert: Failed to generate error message for $eType")
  }

  val StringPattern = """String\("(.*)"\)""".r
}
