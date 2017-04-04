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

import com.datastax.driver.core.{BoundStatement, PreparedStatement, ResultSet, Session}
import com.google.common.util.concurrent.{FutureCallback, Futures, ListenableFuture}

import scala.concurrent.{Future, Promise}

trait StatementPreparationStrategy {
  def prepare(session: Session, raw: String): Future[PreparedStatement]
}
object StatementPreparationStrategy {
  val syncStatementPreparationStrategy = new StatementPreparationStrategy {
    override def prepare(session: Session, raw: String): Future[PreparedStatement] =
      Future.successful(session.prepare(raw))
  }

  implicit val default = syncStatementPreparationStrategy
}


trait StatementExecutionStrategy {
  def execute(session: Session, statement: BoundStatement): Future[ResultSet]
}
object StatementExecutionStrategy {
  import JavaConverters.RichListenableFuture

  val asyncStatementExecutionStrategy = new StatementExecutionStrategy {
    override def execute(session: Session, statement: BoundStatement): Future[ResultSet] =
      session.executeAsync(statement).asScala
  }

  implicit val default = asyncStatementExecutionStrategy
}

object JavaConverters {
  implicit class RichListenableFuture[T](lf: ListenableFuture[T]) {
    def asScala: Future[T] = {
      val p = Promise[T]()
      Futures.addCallback(lf, new FutureCallback[T] {
        def onFailure(t: Throwable): Unit = p failure t
        def onSuccess(result: T): Unit = p success result
      })
      p.future
    }
  }
}
