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

import com.datastax.driver.core.Session
import scala.collection.JavaConverters._

import shapeless._

import scala.concurrent.{ExecutionContext, Future}

trait Query[Input, Output] {
  def apply(input: Input)(implicit ec: ExecutionContext): Future[Iterable[Output]]
}
object Query {

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
    ](rawQuery: String)(
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
      inputBinder: StatementBinder[GenericInput, BindMarkerTypes],
      rowParser: RowParser[GenericOutput, SelectionTypes],
      statementPreparer: StatementPreparationStrategy,
      statementExecutor: StatementExecutionStrategy,
      session: Session
    ) = new Query[Input, Output] {
      val preparedStatementF = statementPreparer.prepare(session, rawQuery)

      override def apply(input: Input)(implicit ec: ExecutionContext) =
        preparedStatementF
          .map(inputBinder.bind(_, inputGeneric.to(input)))
          .flatMap(statementExecutor.execute(session, _))
          .map(_.asScala)
          .map(_.map(rowParser.parse).map(outputGeneric.from))
    }
  }
}
