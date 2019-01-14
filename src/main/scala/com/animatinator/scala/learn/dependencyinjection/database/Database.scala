package com.animatinator.scala.learn.dependencyinjection.database

trait Database {
  def getStoredStrings : List[String]
  def storeString(string : String) : Unit
}
