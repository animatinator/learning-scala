package com.animatinator.scala.learn.dependencyinjection.logging

trait Logger {
  def log(tag : String, message : String) : Unit
}
