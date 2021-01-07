package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine match {
      case Nil => Nil
      case head :: Nil => head :: 1 :: Nil
      case head :: tail =>
        tail.foldLeft(List(head, 1)) { (acc, x) =>
          if (acc.head == x) acc.head :: (acc.tail.head + 1) :: acc.tail.tail
          else x :: 1 :: acc
        }.reverse
    }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(nextLine)
}