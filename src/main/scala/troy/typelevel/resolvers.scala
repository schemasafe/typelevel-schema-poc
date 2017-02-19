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

import shapeless._

@implicitNotFound("Bug alert: DoesTableExists[${T}] was supposed to be auto-derived.")
trait DoesTableExists[T <: String] {
  // Success[Unit] -> exists
  // Error -> Doesn't exist
  type Out <: Result[TableDoesNotExist[T], Unit]
}

object DoesTableExists extends DoesTableExistsLowerPriorityImplicits {
  type Aux[T <: String, O <: Result[TableDoesNotExist[T], Unit]] = DoesTableExists[T] { type Out = O }

  def instance[T <: String, O <: Result[TableDoesNotExist[T], Unit]]: Aux[T, O] =
    new DoesTableExists[T] { override type Out = O }

  implicit def existsIfTableExistsDefined[T <: String](implicit te: TableExists[T]) =
    instance[T, Success[Unit]]
}
trait DoesTableExistsLowerPriorityImplicits {
  implicit def doesNotExistsIfTableExistsNotDefined[T <: String] =
    DoesTableExists.instance[T, Failure[TableDoesNotExist[T]]]
}

@implicitNotFound("Bug alert: DoesColumnExists[${T}] was supposed to be auto-derived.")
trait DoesColumnExists[T <: String, C <: String] {
  // Success[Unit] -> exists
  // Error -> Doesn't exist
  type Out <: Result[ColumnDoesNotExist[T, C], Unit]
}

object DoesColumnExists extends DoesColumnExistsLowerPriorityImplicits {
  type Aux[T <: String, C <: String, O <: Result[ColumnDoesNotExist[T, C], Unit]] = DoesColumnExists[T, C] { type Out = O }

  def instance[T <: String, C <: String, O <: Result[ColumnDoesNotExist[T, C], Unit]]: Aux[T, C, O] =
    new DoesColumnExists[T, C] { override type Out = O }

  implicit def existsIfColumnExistsDefined[T <: String, C <: String](implicit cht: ColumnHasType[T, C]) =
    instance[T, C, Success[Unit]]
}
trait DoesColumnExistsLowerPriorityImplicits {
  implicit def doesNotExistsIfColumnExistsNotDefined[T <: String, C <: String] =
    DoesColumnExists.instance[T, C, Failure[ColumnDoesNotExist[T, C]]]
}


trait SelectionTypeResolver[T <: String, Selection <: HList] {
  type Out <: Result[_, HList] // HList of ColumnType
}
object SelectionTypeResolver extends SelectionTypeResolverLowPriorityImplicits {
  type Aux[T <: String, S <: HList, O <: Result[_, HList]] = SelectionTypeResolver[T, S] { type Out = O }

  def instance[T <: String, S <: HList, O <: Result[_, HList]]: Aux[T, S, O] =
    new SelectionTypeResolver[T, S] { override type Out = O }

  implicit def hNilInstanceSuccess[T <: String](implicit tableExists: DoesTableExists.Aux[T, Success[Unit]]) =
    instance[T, HNil, Success[HNil]]
}

trait SelectionTypeResolverLowPriorityImplicits {
  implicit def hNilInstanceFailure[T <: String](implicit tableDoesNotExist: DoesTableExists.Aux[T, Failure[TableDoesNotExist[T]]]) =
    SelectionTypeResolver.instance[T, HNil, Failure[TableDoesNotExist[T]]]

  // implicit def hConsInstance[T <: String : DoesTableExists, C <: String : DoesColumnExists, ] = instance[T, HNil, HNil]
}
