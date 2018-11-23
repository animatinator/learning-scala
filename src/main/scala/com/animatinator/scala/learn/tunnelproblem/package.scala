package com.animatinator.scala.learn

package object tunnelproblem {
  object TunnelProblem {
    def numberOfFittedObjects(heights : List[Int], objects : List[Int]) : Int = {
      // Returns a pair (unfitted, fitted) of the objects which were and were not fitted into the tunnel.
      def fitObjects(maxHeight : Int, heights : List[Int], objects : List[Int]) : (List[Int], List[Int]) =
        heights match {
          case Nil => (objects, Nil)
          case ceiling :: remainingTunnel =>
            val maxHeightToHere = Math.min(maxHeight, ceiling)
            fitObjects(maxHeightToHere, remainingTunnel, objects) match {
              case (smallest :: rest, fitted) if smallest <= maxHeightToHere => (rest, smallest :: fitted)
              case other => other
            }
        }

      if (heights.isEmpty || objects.isEmpty) 0
      else {
        fitObjects(heights.head, heights, objects.sorted)._2.length
      }
    }
  }
}
