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

object TestSchema {
  implicit val fact1 = TableExists.instance["test"]
  implicit val fact2 = ColumnHasType.instance["test", "x", ColumnType.Text]
  implicit val fact3 = ColumnHasType.instance["test", "y", ColumnType.Int]
  implicit val fact4 = ColumnHasType.instance["test", "z", ColumnType.List[ColumnType.Text]]
}

object SelectSpec {
    import Matchers._
    import TestSchema._
    import Operator._

    Query.select[SelectStatement[
      "x" :: "y" :: "z" :: HNil, // Adding unkown column shows compile error: Column "W" does not exist in table "test"
      "test", // Changing this to unknown table shows compile error: Table "foo" does not exist.
      Relation["x", Equals] :: Relation["y", Equals] :: Relation["z", Contains] :: HNil
    ]]()

    // t[Query[]]
}
