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

import scala.concurrent.Future

trait Query[S <: DataManipulationStatement] {
  type Input // case class
  type Output // case class

  def apply(input: Input): Future[Output]
}
object Query {

  def instance[I, O, S <: DataManipulationStatement](impl: I => Future[O]) =
    new Query[S] {
      override type Input = I
      override type Output = O

      override def apply(input: I): Future[O] = impl(input)
    }

  def select[Input, Output, Statement <: SelectStatement[_, _, _]] = new {
    def apply[
      Selection <: HList,
      Table <: String,
      Relations <: HList,
      MaybeSelectionTypes <: Result[_, HList],
      SelectionTypes <: HList, // HList of ColumnType
      MaybeBindMarkerTypes <: Result[_, HList],
      BindMarkerTypes <: HList, // HList of ColumnType
      GenericInput <: HList,
      GenericOutput <: HList
    ]()(
      implicit
      getTableName: GetTableName.Aux[Statement, Table],
      getSelection: GetSelection.Aux[Statement, Selection],
      getRelations: GetRelations.Aux[Statement, Relations],
      tryResolveSelectionType: SelectionTypeResolver.Aux[Table, Selection, MaybeSelectionTypes],
      selectionTypes: GetOrElseFail.Aux[MaybeSelectionTypes, SelectionTypes],
      tryResolveBindMarkerTypes: BindMarkerTypesResolver.Aux[Table, Relations, MaybeBindMarkerTypes],
      bindMarkerTypes: GetOrElseFail.Aux[MaybeBindMarkerTypes, BindMarkerTypes],
      inputGeneric: Generic.Aux[Input, GenericInput],
      outputGeneric: Generic.Aux[Output, GenericOutput],
      // GenericInput should be matched with BindMarkerTypes
      // GenericOutput should be matched with SelectionTypes
//      statementPreparer: ???,
      inputBinder: StatementBinder[GenericInput, BindMarkerTypes]
//      rowParser: ???
    ) = instance[Input, Output, Statement] { input =>
      val genInput = inputGeneric.to(input)
      val genOutput: GenericOutput = ???

      Future.successful(outputGeneric.from(genOutput))
    }
  }
}
