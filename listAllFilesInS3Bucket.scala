package com.grimey

import com.amazonaws.services.s3.model.{ ObjectListing, S3ObjectSummary }
import scala.annotation.tailrec

class Test {
  private def buildFilePathsSummary(
    bucketName: String,
    prefix: String
  ): List[S3ObjectSummary] = {
    @tailrec
    def loop(
      listing: ObjectListing,
      summaries: List[S3ObjectSummary] = List()
    ): List[S3ObjectSummary] =
      if (listing.isTruncated) {
        val nextFilePathBatch: ObjectListing =
          SerializableS3Client.client.listNextBatchOfObjects(listing)

        loop(
          nextFilePathBatch,
          summaries ++ listing.getObjectSummaries.asScala.toList
        )
      } else summaries ++ listing.getObjectSummaries.asScala.toList

    val initialFilePathsListing: ObjectListing =
      SerializableS3Client.client.listObjects(bucketName, prefix)

    loop(initialFilePathsListing)
  }
}

import com.amazonaws.services.s3.AmazonS3ClientBuilder

object SerializableS3Client {
  val client =
    AmazonS3ClientBuilder.standard().withRegion("us-east-1").build()
}
