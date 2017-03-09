package com.grimey.dataStructures

import scala.annotation.tailrec

class GrimeyList(val head: GrimeyNode) {
  def add(data: Int): Unit = addToTail(head, data)

  @tailrec
  private def addToTail(node: GrimeyNode, data: Int): Unit = {
    if (node.nextGrimeyNode.isEmpty)
      node.nextGrimeyNode = Some(new GrimeyNode(data, None))
    else addToTail(node.nextGrimeyNode.get, data)
  }

  def forEach(applyFunction: (GrimeyNode) => Unit): Unit = {
    ???
  }
}

// Todo: Make this a case class.
class GrimeyNode(val data: Int, var nextGrimeyNode: Option[GrimeyNode])