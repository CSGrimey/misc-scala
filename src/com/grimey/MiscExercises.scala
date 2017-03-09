package com.grimey

import scala.annotation.tailrec

abstract class GList[T] {
  def head: T

  def tail: GList[T]

  def isEmpty: Boolean

  def length: Int

  def setHead(item: T): GList[T] = new GrimeyList(item, this)

  def ::[U >: T](item: T): GList[U] = new GrimeyList(item, this.asInstanceOf[GList[U]])
}

object GList {
  def apply[T](items: T*): GList[T] = {
    var list: GList[T] = NilGrimeyList.asInstanceOf[GList[T]]

    for (index <- 0 until items.length reverse) list = items(index) :: list

    list
  }
}

class GrimeyList[T](val head: T, val tail: GList[T]) extends GList[T] {
  def isEmpty = false

  def length: Int = tail.length + 1

  override def toString: String = s"$head $tail"
}

object NilGrimeyList extends GList[Nothing] {
  def head: Nothing = throw new Exception("Head of list is empty")

  def tail: GList[Nothing] = throw new Exception("Tail of list is empty")

  def isEmpty = true

  def length = 0

  override def toString = ""
}

sealed trait GrimeyLinkedList[+T] {
  def size: Int

  def map[U](applyFunction: T => U): GrimeyLinkedList[U] = foldRight(GrimeyLinkedList[U]()) {
    (item, accumulator) => GrimeyNode(applyFunction(item), accumulator)
  }

  @tailrec final def foldLeft[U](accumulator: U)(func: (U, T) => U): U = {
    this match {
      case GrimeyNode(head, tail) => {
        val current = func(accumulator, head)
        tail.foldLeft(current)(func)
      }
      case Empty => accumulator
    }
  }

  def foldRight[U](accumulator: U)(func: (T, U) => U): U = {
    reverse().foldLeft(accumulator)((accumulator, item) => func(item, accumulator))
  }

  def reverse(): GrimeyLinkedList[T] = {
    foldLeft(GrimeyLinkedList[T]()) {
      (accumulator, item) => GrimeyNode(item, accumulator)
    }
  }

  def filter(func: (T) => Boolean): GrimeyLinkedList[T] = {
    foldRight(GrimeyLinkedList[T]()) {
      (item, accumulator) =>
        if (func(item)) GrimeyNode(item, accumulator)
        else accumulator
    }
  }

  def ::[U >: T](element: U): GrimeyLinkedList[U] = GrimeyNode(element, this)

  def :::[U >: T](prefixList: GrimeyLinkedList[U]): GrimeyLinkedList[U] = {
    @tailrec def helper(accumulator: GrimeyLinkedList[U], other: GrimeyLinkedList[U]): GrimeyLinkedList[U] = {
      other match {
        case GrimeyNode(head, tail) => helper(head :: accumulator, tail)
        case Empty => accumulator
      }
    }

    helper(this, prefixList.reverse())
  }

  @tailrec final def foreach(func: (T) => Unit): Unit = {
    this match {
      case GrimeyNode(head, tail) => {
        func(head)
        foreach(func)
      }
      case Empty => {}
    }
  }

  @tailrec final def find(predicate: (T) => Boolean): Option[T] = {
    this match {
      case GrimeyNode(head, tail) => {
        if (predicate(head)) Some(head)
        else tail.find(predicate)
      }
      case Empty => None
    }
  }
}

object GrimeyLinkedList {
  def apply[T](items: T*): GrimeyLinkedList[T] = {
    if (items.isEmpty) Empty
    else GrimeyNode(items.head, apply(items.tail: _*))
  }
}

case class GrimeyNode[+T](val head: T, val tail: GrimeyLinkedList[T]) extends GrimeyLinkedList[T] {
  val size: Int = 1 + tail.size
}

case object Empty extends GrimeyLinkedList[Nothing] {
  val size = 0
}