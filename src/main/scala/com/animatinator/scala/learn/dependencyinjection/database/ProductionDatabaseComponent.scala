package com.animatinator.scala.learn.dependencyinjection.database

import com.animatinator.scala.learn.dependencyinjection.logging.LoggingComponent

trait ProductionDatabaseComponent extends DatabaseComponent {
  this : LoggingComponent =>
  override lazy val database = new ProductionDatabase(logger)
}
