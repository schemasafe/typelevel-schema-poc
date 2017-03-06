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

import shapeless._

sealed trait DataManipulationStatement

// `select <Selection> from <Table> where <Relations>`
sealed trait SelectStatement[
  Selection <: HList, // of Column Names encoded as literal-type strings
  Table <: String, // table name, encoded as literal-type string
  Relations <: HList // of Relations
] extends DataManipulationStatement

// `<ColumnName> <Op> ?`, ex: `postId == ?`
sealed trait Relation[ColumnName <: String, Op <: Operator]

sealed trait Operator
object Operator {
  sealed trait Equals extends Operator // ==
  sealed trait Contains extends Operator // Used for collections only
}

sealed trait ColumnType
object ColumnType {
  sealed trait Native extends ColumnType
  sealed trait Text extends Native
  sealed trait Int extends Native

  trait Collection extends ColumnType
  trait List[T <: Native] extends Collection
}
