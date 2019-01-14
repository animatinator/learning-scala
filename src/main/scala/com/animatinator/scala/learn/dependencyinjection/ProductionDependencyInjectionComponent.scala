package com.animatinator.scala.learn.dependencyinjection

import com.animatinator.scala.learn.dependencyinjection.database.ProductionDatabaseComponent
import com.animatinator.scala.learn.dependencyinjection.logging.ConsoleLoggerComponent
import com.animatinator.scala.learn.dependencyinjection.users.DatabaseUserAccessorComponent

/**
  * This is a concrete version of UserAccessorComponent with all its dependencies filled in by concrete versions of the
  * components on which it depends.
  */
class ProductionDependencyInjectionComponent
  extends DatabaseUserAccessorComponent with ProductionDatabaseComponent with ConsoleLoggerComponent {

}
