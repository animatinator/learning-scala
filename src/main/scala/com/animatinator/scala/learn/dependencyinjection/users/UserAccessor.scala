package com.animatinator.scala.learn.dependencyinjection.users

trait UserAccessor {
  def getUsers : List[String]
  def addUser(user : String) : Unit
}
