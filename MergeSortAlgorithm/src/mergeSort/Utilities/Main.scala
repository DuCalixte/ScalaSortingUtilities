package mergeSort.Utilities

import MergeSort.genericMergeSort

object Main extends App{
  //import MergeSort._
  
  var target = List(1, 9, 5, 0, 4, 8, 2, 7, 6)
  println()
  target.map(x => print(x + " "))
  println()
  var destination = genericMergeSort(target)
  println()
  destination.map(x => print(x + " "))
  println()

}