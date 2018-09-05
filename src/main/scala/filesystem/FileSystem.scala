package filesystem

import java.util.Scanner

import commands.Command
import files.Directory

object FileSystem extends App {

  val root = Directory.ROOT
  var state = State(root, root)
  val scanner = new Scanner(System.in)

  while(true){
    state.show
    val input = scanner.nextLine()
    state = Command.from(input).apply(state)
  }

}