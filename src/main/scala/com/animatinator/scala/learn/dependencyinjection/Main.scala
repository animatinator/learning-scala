package com.animatinator.scala.learn.dependencyinjection

import com.animatinator.scala.learn.dependencyinjection.users.UserAccessor

object Main {
  def main(args : Array[String]): Unit = {
    val userAccessor = getUserAccessor
    userAccessor addUser "David"
    userAccessor addUser "Bob"
    userAccessor addUser "Jill"
    val users = userAccessor getUsers;
    println(users)
  }

  def getUserAccessor : UserAccessor = new ProductionDependencyInjectionComponent userAccessor
}
