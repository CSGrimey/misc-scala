package com.grimey.spark

import org.apache.spark.SparkConf
import org.apache.spark.sql.{ SQLContext, SparkSession }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, Suite }

trait SharedSQLContext extends BeforeAndAfterAll with BeforeAndAfterEach { self: Suite =>
  @transient private var _sc: SQLContext = _

  def sqlContext: SQLContext = _sc

  var conf = new SparkConf(false)

  protected def initializeContext(): Unit = {
    if (null == _sc) {
      val sparkSession = SparkSession.builder
        .appName("Shared spark tests")
        .master("local[*]")
        .getOrCreate()

      sparkSession.sqlContext.sparkContext.setLogLevel("ERROR")

      _sc = sparkSession.sqlContext
    }
  }

  override def beforeAll() {
    super.beforeAll()
    initializeContext()
  }

  override def afterAll() {
    super.afterAll()
  }

  protected override def beforeEach(): Unit = {
    super.beforeEach()
  }

  protected override def afterEach(): Unit = {
    super.afterEach()
  }
}
