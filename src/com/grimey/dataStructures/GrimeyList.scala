package com.grimey.dataStructures

import scala.annotation.tailrec

abstract class GrimeyList[+T] {
  final def map[U](functionToApply: T => U): GrimeyList[U] =
    foldRight(GrimeyList[U]()) {
      (item, accumulator) => GrimeyNode(functionToApply(item), accumulator)
  }

  @tailrec final def foldLeft[U](accumulator: U)(functionToApply: (U, T) => U) : U = {
    this match {
      case Empty => accumulator
      case GrimeyNode(head, tail) =>
        tail.foldLeft(functionToApply(accumulator, head))(functionToApply)
    }
  }

  def reverse(): GrimeyList[T] =
    foldLeft(GrimeyList[T]()) { (accumulator, item) => GrimeyNode(item, accumulator) }

  def foldRight[U](accumulator: U)(functionToApply: (T, U) => U): U =
    reverse().foldLeft(accumulator)((accumulator, item) => functionToApply(item, accumulator))

  def filter(functionToApply: (T) => Boolean): GrimeyList[T] = {
    foldRight(GrimeyList[T]()) {
      (item, accumulator) =>
        if (functionToApply(item)) GrimeyNode(item, accumulator)
        else accumulator
    }
  }

  def size: Int

  def ::[U >: T](element: U): GrimeyList[U] = GrimeyNode(element, this)

  def :::[U >: T](prefix: GrimeyList[U]): GrimeyList[U] = {
    @tailrec def helper(accumulator: GrimeyList[U], other: GrimeyList[U]): GrimeyList[U] = {
      other match {
        case Empty => accumulator
        case GrimeyNode(head, tail) => helper(head :: accumulator, tail)
      }
    }

    helper(this, prefix.reverse())
  }

  def foreach(functionToApply: (T) => Unit): Unit = {
    @tailrec def loop(items: GrimeyList[T]): Unit = {
      items match {
        case Empty => {}
        case GrimeyNode(head, tail) => {
          functionToApply(head)

          loop(tail)
        }
      }
    }

    loop(this)
  }

  @tailrec final def find(predicate: (T) => Boolean): Option[T] = {
    this match {
      case Empty => None
      case GrimeyNode(head, tail) => {
        if (predicate(head)) Some(head)
        else tail.find(predicate)
      }
    }
  }
}

object GrimeyList {
  // * is the same as params in C#
  def apply[T](items: T*): GrimeyList[T] = {
    if (items.isEmpty) Empty
    // _* tells the compiler to not add list as first parameter, but as all the parameters which are all the same type.
    else GrimeyNode(items.head, apply(items.tail: _*))
  }
}

case class GrimeyNode[T](head: T, tail: GrimeyList[T]) extends GrimeyList[T] {
  val size = tail.size + 1
}

case object Empty extends GrimeyList[Nothing] {
  val size = 0
}