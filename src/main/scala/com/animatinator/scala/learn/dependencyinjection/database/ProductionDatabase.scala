package com.animatinator.scala.learn.dependencyinjection.database

import com.animatinator.scala.learn.dependencyinjection.logging.Logger

class ProductionDatabase(logger : Logger) extends Database {
  val TAG = "ProductionDatabase"
  var strings : List[String] = Nil

  override def getStoredStrings: List[String] = {
    logger.log(TAG, "Getting all stored strings")
    strings
  }

  override def storeString(string: String): Unit = {
    logger.log(TAG, s"Storing string: $string")
    strings = string :: strings
  }
}
