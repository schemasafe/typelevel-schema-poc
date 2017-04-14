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
package troy.api

import scala.meta._
import troy.parsers.Schema


class SchemaFromString(schema: String)

class schema extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    def log[T](t: T): T = { println(t); t}
    defn match {
      case q"..$mods object $name extends SchemaFromString(${Lit(rawSchema: String)})" =>
        val facts = Schema.parseToTypelevel(rawSchema).right.get.to[scala.collection.immutable.Seq]

        log(q"""..$mods object $name {
          import _root_.troy.typelevel._
          import _root_.shapeless._

          ..$facts
        }""")
      case _ =>
        abort(???, "")
    }
  }
}
