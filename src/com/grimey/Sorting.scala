package com.grimey

import scala.annotation.tailrec

object Sorting {
  private[grimey] def sortProductBasedOnOtherList[T <: Product](productList: List[T], productKeyList: List[String]): List[T] = {
    val productKeysWithIndex = productKeyList

    productList.foldLeft(List[T]()){(aggregate, product) => {
      val keyIndex = productKeysWithIndex.indexOf(product.key)

      val (previousProducts, nextProducts) = aggregate.partition((product) => productKeysWithIndex.indexOf(product.key) < keyIndex)

      (previousProducts :+ product) ++ nextProducts
    }}
  }

  private[grimey] def reverse(text: String): String = {
    @tailrec def helper(text: String, accumulator: String): String =
      if (text.isEmpty) accumulator
      else helper(text.substring(1), text.head + accumulator)

    helper(text, accumulator = "")
  }
}