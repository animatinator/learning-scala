package com.animatinator.scala.learn.dependencyinjection.users

import com.animatinator.scala.learn.dependencyinjection.database.DatabaseComponent
import com.animatinator.scala.learn.dependencyinjection.logging.LoggingComponent

trait DatabaseUserAccessorComponent extends UserAccessorComponent {
  this : DatabaseComponent with LoggingComponent =>

  override lazy val userAccessor: UserAccessor = new DatabaseUserAccessor(database, logger)
}
