package com.grimey

object MiscExercisesMain extends App {
  override def main(args: Array[String]): Unit = {
    val productKeyList = List("Nexus", "Kindle", "Ipad")
    val productList = List(Product("Ipad"), Product("Nexus"), Product("Kindle"))

    val sortedProducts = sortProductBasedOnOtherList(productList, productKeyList)

    println(s"Product keys = $productKeyList")
    println(s"Products = $productList")

    println(s"Sorted products = $sortedProducts")

    println(makeRow(3, 1, 8))
    println(makeRow(3, 1, 9))
    println(makeRow(3, 2, 10))
  }

  private def sortProductBasedOnOtherList(productList: List[Product], productKeyList: List[String]): List[Product] = {
    val productKeysWithIndex = productKeyList

    productList.foldLeft(List[Product]()){(aggregate, product) => {
      val keyIndex = productKeysWithIndex.indexOf(product.key)

      val (previousProducts, nextProducts) = aggregate.partition((product) => productKeysWithIndex.indexOf(product.key) < keyIndex)

      (previousProducts :+ product) ++ nextProducts
    }}
  }

  private def makeRow(small: Int, big: Int, goal: Int): Boolean = {
    val smallBricks: List[SmallBrick] = if (small > 0) (1 to small).toList.map(index => new SmallBrick())
                                        else List[SmallBrick]()

    val smallBricksLength: Int = calculateBricksLength(smallBricks)

    val bigBricks: List[BigBrick] = if (big > 0) (1 to big).toList.map(index => new BigBrick())
                                    else List[BigBrick]()

    val bigBricksLength: Int = calculateBricksLength(bigBricks)

    (smallBricksLength + bigBricksLength) >= goal
  }

  private def calculateBricksLength(bricks: List[Brick]): Int = bricks.map(_.size).sum
}

case class Product(val key: String)

trait Brick {
  val size: Int
}

class SmallBrick extends Brick {
  override val size: Int = 1
}

class BigBrick extends Brick {
  override val size: Int = 5
}