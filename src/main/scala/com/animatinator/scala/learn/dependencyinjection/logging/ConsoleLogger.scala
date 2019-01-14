package com.animatinator.scala.learn.dependencyinjection.logging

class ConsoleLogger extends Logger {
  override def log(tag: String, message: String): Unit = println(s"[$tag]: $message")
}
