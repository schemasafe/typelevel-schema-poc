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
  @implicitNotFound("Bug alert: SelectionTypeResolver.Aux[${T}, ${S}, ${O}] was supposed to be auto-derived.")
  type Aux[T <: String, S <: HList, O <: Result[_, HList]] = SelectionTypeResolver[T, S] { type Out = O }

  def instance[T <: String, S <: HList, O <: Result[_, HList]]: Aux[T, S, O] =
    new SelectionTypeResolver[T, S] { override type Out = O }

  implicit def hNilInstanceSuccess[T <: String](implicit tableExists: DoesTableExists.Aux[T, Success[Unit]]) =
    instance[T, HNil, Success[HNil]]

  implicit def hConsInstanceSuccess[T <: String, HHead <: String, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType[T, HHead],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, HHead :: HTail, Success[hConsType.T :: HTailColumnTypes]]
}

trait SelectionTypeResolverLowPriorityImplicits extends SelectionTypeResolverLowerPriorityImplicits {
  import SelectionTypeResolver.{instance, Aux}

  implicit def hConsInstanceFailure[T <: String, HHead <: String, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, HHead :: HTail, Failure[ColumnDoesNotExist[T, HHead]]]

  implicit def hConsPropagateTailFailure[T <: String, HHead <: String, HTail <: HList, E <: Error[_]](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hTail: Aux[T, HTail, Failure[E]],
  ) = instance[T, HHead :: HTail, Failure[E]]
}

trait SelectionTypeResolverLowerPriorityImplicits {
  import SelectionTypeResolver.{instance, Aux}

  implicit def tableDoesntExistFailure[T <: String, S <: HList](
    implicit tableExists: DoesTableExists.Aux[T, Failure[TableDoesNotExist[T]]]
  ) = instance[T, S, Failure[TableDoesNotExist[T]]]
}

trait BindMarkerTypesResolver[T <: String, Relations <: HList /* of Relation[_] */] {
  type Out <: Result[_, HList] // HList of ColumnType
}
object BindMarkerTypesResolver extends BindMarkerTypesResolverLowPriorityImplicits {
  @implicitNotFound("Bug alert: BindMarkerTypesResolver.Aux[${T}, ${Rs}, ${O}] was supposed to be auto-derived.")
  type Aux[T <: String, Rs <: HList, O <: Result[_, HList]] = BindMarkerTypesResolver[T, Rs] { type Out = O }

  def instance[T <: String, Rs <: HList, O <: Result[_, HList]]: Aux[T, Rs, O] =
    new BindMarkerTypesResolver[T, Rs] { override type Out = O }

  implicit def hNilInstanceSuccess[T <: String](implicit tableExists: DoesTableExists.Aux[T, Success[Unit]]) =
    instance[T, HNil, Success[HNil]]

  implicit def hConsNativeInstanceSuccess[T <: String, HHeadColumnName <: String, HHeadColumnType <: ColumnType.Native, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType.Aux[T, HHeadColumnName, HHeadColumnType],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, Relation[HHeadColumnName, Operator.Equals] :: HTail, Success[HHeadColumnType :: HTailColumnTypes]]

  implicit def hConsListInstanceSuccess[T <: String, HHeadColumnName <: String, HHeadListTypeParam <: ColumnType.Native, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType.Aux[T, HHeadColumnName, ColumnType.List[HHeadListTypeParam]],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, Relation[HHeadColumnName, Operator.Contains] :: HTail, Success[HHeadListTypeParam :: HTailColumnTypes]]

  implicit def hConsNativeContainsInstanceFailure[T <: String, HHeadColumnName <: String, HHeadColumnType <: ColumnType.Native, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType.Aux[T, HHeadColumnName, HHeadColumnType],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, Relation[HHeadColumnName, Operator.Contains] :: HTail, Failure[NativeColumnDoesNotSupportContainsOperator[T, HHeadColumnName]]]

  implicit def hConsListCollectionInstanceFailure[T <: String, HHeadColumnName <: String, HHeadListTypeParam <: ColumnType.Native, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType.Aux[T, HHeadColumnName, ColumnType.List[HHeadListTypeParam]],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, Relation[HHeadColumnName, Operator.Equals] :: HTail, Failure[CollectionColumnDoesNotSupportEqualsOperator[T, HHeadColumnName]]]
}

trait BindMarkerTypesResolverLowPriorityImplicits extends BindMarkerTypesResolverLowerPriorityImplicits {
  import BindMarkerTypesResolver.{instance, Aux}

  implicit def hConsColumnNotFoundInstanceFailure[T <: String, HHeadColumnName <: String, Op <: Operator, HTail <: HList, HTailColumnTypes <: HList](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hTail: Aux[T, HTail, Success[HTailColumnTypes]],
  ) = instance[T, Relation[HHeadColumnName, Op] :: HTail, Failure[ColumnDoesNotExist[T, HHeadColumnName]]]

  // implicit def hConsPropagateTailFailure[T <: String, HHead <: String, HTail <: HList, E <: Error[_]](
  //   implicit
  //   tableExists: DoesTableExists.Aux[T, Success[Unit]],
  //   hTail: Aux[T, HTail, Failure[E]],
  // ) = instance[T, HHead :: HTail, Failure[E]]

  implicit def hConsNativePropagateTailFailure[T <: String, HHeadColumnName <: String, HHeadColumnType <: ColumnType.Native, HTail <: HList, E <: Error[_]](
    implicit
    tableExists: DoesTableExists.Aux[T, Success[Unit]],
    hConsType: ColumnHasType.Aux[T, HHeadColumnName, HHeadColumnType],
    hTail: Aux[T, HTail, Failure[E]],
  ) = instance[T, Relation[HHeadColumnName, Operator.Equals] :: HTail, Failure[E]]
}

trait BindMarkerTypesResolverLowerPriorityImplicits {
  import BindMarkerTypesResolver.{instance, Aux}
  
  implicit def tableDoesntExistFailure[T <: String, S <: HList](
    implicit tableExists: DoesTableExists.Aux[T, Failure[TableDoesNotExist[T]]]
  ) = instance[T, S, Failure[TableDoesNotExist[T]]]
}
