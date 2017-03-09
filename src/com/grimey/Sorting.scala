package com.grimey

object Sorting {
  private[grimey] def sortProductBasedOnOtherList[T <: Product](productList: List[T], productKeyList: List[String]): List[T] = {
    val productKeysWithIndex = productKeyList

    productList.foldLeft(List[T]()){(aggregate, product) => {
      val keyIndex = productKeysWithIndex.indexOf(product.key)

      val (previousProducts, nextProducts) = aggregate.partition((product) => productKeysWithIndex.indexOf(product.key) < keyIndex)

      (previousProducts :+ product) ++ nextProducts
    }}
  }
}