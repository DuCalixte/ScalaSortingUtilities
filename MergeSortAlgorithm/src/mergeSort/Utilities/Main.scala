package mergeSort.Utilities

import MergeSort._

object Main extends App{
  
  var target = List(1, 9, 5, 0, 4, 8, 2, 7, 6)
  println()
  target.map(x => print(x + " "))
  println()
  var destination = genericMergeSort(target)
  println()
  destination.map(x => print(x + " "))
  println()
  
  var final_destination = mergeSort(target)
  println()
  final_destination.map(x => print(x + " "))
  println()

}