package edu.knoldus

import org.apache.log4j.Logger

object Activity {

  def main(args: Array[String]): Unit = {
    val logger = Logger.getLogger(this.getClass)
    val activity = new Activity
    val firstElement = "a"
    val secondElement = "b"
    val thirdElement = "c"
    val inputList: List[String] = List(firstElement, secondElement, thirdElement)
    logger.info("\n" + activity.length(inputList))
    val fourthElement = "d"
    val fifthElement = "e"
    val sixthElement = "f"
    val firstList = inputList
    val secondList = List(fourthElement, fifthElement, sixthElement)
    logger.info("\n" + activity.concateList(firstList, secondList))
    val subListElement1 = "a"
    val subListElement2 = "c"
    val subList = List(subListElement1, subListElement2)
    logger.info("\n" + activity.hasSubsequence(inputList, subList))

  }
}

class Activity {

  def length[A](inputList: List[A]): Int = {
    inputList.foldRight(0)((inputListElement, lengthOfList) => (lengthOfList + 1))
  }

  def concateList[A](firstList: List[A], secondList: List[A]): List[A] = {
    secondList match {
      case head :: tail if (secondList != Nil) => concateList(firstList :+ head, tail)
      case Nil => firstList
    }
  }

  def hasSubsequence[A](inputList: List[A], subList: List[A]): Boolean = {
    val subListLength = subList.length

    def slicedList(inputListLength: Int, subListLength: Int): Boolean = {
      if (inputList.slice(inputListLength, subListLength) == subList) {
        true
      }
      else if (inputListLength < inputList.length) {
        slicedList(inputListLength + 1, subListLength)
      }
      else {
        false
      }
    }

    slicedList(0, subListLength)
  }
}


