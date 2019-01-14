package com.animatinator.scala.learn.dependencyinjection.logging

trait ConsoleLoggerComponent extends LoggingComponent {
  override lazy val logger: Logger = new ConsoleLogger
}
