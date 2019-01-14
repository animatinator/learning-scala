package com.animatinator.scala.learn.dependencyinjection.users

import com.animatinator.scala.learn.dependencyinjection.database.Database
import com.animatinator.scala.learn.dependencyinjection.logging.Logger

class DatabaseUserAccessor(database : Database, logger : Logger) extends UserAccessor {

  private val TAG = "DatabaseUserAccessor"

  override def getUsers: List[String] = {
    logger.log(TAG, "Requesting user list")
    database.getStoredStrings
  }

  override def addUser(user: String): Unit = {
    logger.log(TAG, s"Add user: $user")
    database.storeString(user)
  }
}
