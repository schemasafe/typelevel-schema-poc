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

import shapeless.HList
import singleton.ops.XString

trait GetTableName[S <: DataManipulationStatement] {
  type Out <: XString
}
object GetTableName {
  type Aux[S <: DataManipulationStatement, T <: XString] =
    GetTableName[S] { type Out = T }

  def instance[S <: DataManipulationStatement, T <: XString]: Aux[S, T] =
    new GetTableName[S] { override type Out = T }

  implicit def select[Selection <: HList, Table <: XString, Relations <: HList] = instance[
    SelectStatement[Selection, Table, Relations],
    Table
  ]
}

trait GetSelection[S <: DataManipulationStatement] {
  type Out <: HList
}
object GetSelection {
  type Aux[S <: DataManipulationStatement, O <: HList] =
    GetSelection[S] { type Out = O }

  def instance[S <: DataManipulationStatement, O <: HList]: Aux[S, O] =
    new GetSelection[S] { override type Out = O }

  implicit def select[Selection <: HList, Table <: XString, Relations <: HList] = instance[
    SelectStatement[Selection, Table, Relations],
    Selection
  ]
}

trait GetRelations[S <: DataManipulationStatement] {
  type Out <: HList
}
object GetRelations {
  type Aux[S <: DataManipulationStatement, O <: HList] =
    GetRelations[S] { type Out = O }

  def instance[S <: DataManipulationStatement, O <: HList]: Aux[S, O] =
    new GetRelations[S] { override type Out = O }

  implicit def select[Selection <: HList, Table <: XString, Relations <: HList] = instance[
    SelectStatement[Selection, Table, Relations],
    Relations
  ]
}
