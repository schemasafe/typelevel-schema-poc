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

trait Query[S <: DataManipulationStatement] {
 type Input <: HList
 type Output <: HList
}
object Query {

  def instance[S <: DataManipulationStatement, I <: HList, O <: HList] =
    new Query[S] {
      override type Input = I
      override type Output = O
    }

  def select[Statement <: SelectStatement[_, _, _]] = new {
    def apply[
      Selection <: HList,
      Table <: String,
      Relations <: HList,
      MaybeSelectionTypes <: Result[_, HList],
      SelectionTypes <: HList
    ]()(
      implicit
      getTableName: GetTableName.Aux[Statement, Table],
      getSelection: GetSelection.Aux[Statement, Selection],
      getRelations: GetRelations.Aux[Statement, Relations],
      tryResolveSelectionType: SelectionTypeResolver.Aux[Table, Selection, MaybeSelectionTypes],
      query: GetOrElseFail.Aux[MaybeSelectionTypes, SelectionTypes],
    ) = instance[Statement, HNil, SelectionTypes]
  }
}
