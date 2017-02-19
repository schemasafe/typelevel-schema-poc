package troy.typelevel

import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import shapeless._

sealed trait Error[M <: HList]

sealed trait Result[+E, +T]
sealed trait Success[+T] extends Result[Nothing, T]
sealed trait Failure[E <: Error[_]] extends Result[E, Nothing]

trait GetOrElseFail[R] {
  type Out
}
object GetOrElseFail {
  @implicitNotFound("Bug alert: implicit not found GetOrElseFail.Aux[${R}, ${O}]")
  type Aux[R, O] = GetOrElseFail[R] { type Out = O }

  def apply[R](implicit instance: GetOrElseFail[R]): Aux[R, instance.Out] = instance

  def instance[R, O]: Aux[R, O] = new GetOrElseFail[R] { override type Out = O }

  implicit def getEitherIfRight[T] = instance[Success[T], T]
  implicit def failEitherIfLeft[E <: Error[_]]: Aux[Failure[E], Nothing] = macro GetOrElseFailMacro.fail[E]
}

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

 object GetOrElseFailMacro {
  import Show._
  // implicit def showHNil = instance[HNil]("")
  // implicit def showCons[S: ValueOf, Tail <: HList](implicit showTail: Show[Tail]) = instance[S :: Tail](valueOf[S] + showTail.message)

  def fail[E <: Error[_]](c: Context)(implicit eType: c.WeakTypeTag[E]) = {
    // c.abort(c.enclosingPosition, Show[M].message)
    val parent = eType.tpe.baseClasses(1)
    // Really ugly hack, till I figure out how to fix implicits defined above
    c.abort(c.enclosingPosition, show(c)(eType.tpe.baseType(parent).typeArgs.head))
  }

  def show(c: Context)(typ: c.universe.Type): String = typ match {
    case t if t <:< c.typeOf[HNil]               => ""
    case t if t <:< c.typeOf[shapeless.::[_, _]] => typ.typeArgs.map(show(c)).mkString
    case t                                       => trim(t.toString)
  }

  def trim(typRepr: String): String = typRepr match {
    case StringPattern(str) => str
    case other              => other
  }

  val StringPattern = """String\("(.*)"\)""".r
}

trait Show[E] {
  def message: String
}
object Show {
  def instance[E](msg: String) = new Show[E]{ override val message = msg }
  def apply[E](implicit show: Show[E]) = show

  implicit def tableDoesNotExist[T <: String: ValueOf] = instance[TableDoesNotExist[T]](s"Table ${valueOf[T]} doesn't exist")
  implicit def unknownError[E <: Error[_]] = instance[E]("Bad Query, but we are unable to detect the cause!")
}


class TableDoesNotExist[T: ValueOf] extends Error["Table " :: T :: " does not exist." :: HNil]
class ColumnDoesNotExist[T: ValueOf, C: ValueOf] extends Error["Column " :: C :: " does not exist in table " :: T :: HNil]
