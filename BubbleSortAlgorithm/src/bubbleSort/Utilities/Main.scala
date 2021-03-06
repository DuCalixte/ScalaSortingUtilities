package bubbleSort.Utilities

object Main extends App{
  import BubbleSort._
  
  val array = Array(1, 0, 5, 3, 8, 4, 9)
  println()
  array.map(x => print(x + " "))
  println()
  bubbleSort(array)
  println()
  array.map(x => print(x + " "))
  println()
  
  var target = List(1, 9, 5, 0, 4, 8, 2, 7, 6)
  println()
  target.map(x => print(x + " "))
  println()
  var destination = bubbleSort(target)
  println()
  destination.map(x => print(x + " "))
  println()

}