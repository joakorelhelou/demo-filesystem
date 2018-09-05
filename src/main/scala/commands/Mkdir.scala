package commands

import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists.")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separators!")
    } else if (checkIlegal(name)) {
      state.setMessage(name + ": illegal entry name!")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIlegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {

    def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        /*
        /a/b
          /c
          /d
        current dir /a
          path = ["b"]
        */
        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd

    // 1 . All the directories in the full path
    val allDirsInPath = wd.getAllFoldersInPath

    //2 . Create new directory entry in wd
    val newDir = Directory.empty(wd.path, name)

    //3. update the direcotry structure
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    //3. Dind new working directory INSTANCE
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)

  }
}
