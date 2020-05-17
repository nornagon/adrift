package adrift

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen
import com.sun.nio.file.SensitivityWatchEventModifier
import javax.sound.sampled.AudioSystem

import scala.util.Random

object Main {
  def onFileChanged(path: Path)(f: (Path) => Unit): Unit = {
    val watcher = FileSystems.getDefault.newWatchService()
    val keyToPath = collection.mutable.HashMap.empty[WatchKey, Path]
    val depth = 0
    Files.find(path, depth, (_: Path, attrs: BasicFileAttributes) => { attrs.isDirectory }).forEach((path) => {
      keyToPath.put(path.register(
        watcher,
        Array[WatchEvent.Kind[_]](
          StandardWatchEventKinds.ENTRY_CREATE,
          StandardWatchEventKinds.ENTRY_DELETE,
          StandardWatchEventKinds.ENTRY_MODIFY
        ),
        SensitivityWatchEventModifier.HIGH
      ), path)
    })
    val t = new Thread(() => {
      while (true) {
        val k = watcher.take()
        val root = keyToPath(k)
        val es = k.pollEvents()
        import scala.collection.JavaConverters._
        for (e <- es.asScala) {
          e.context() match {
            case p: Path =>
              val fullPath = root.resolve(p)
              f(fullPath)
          }
        }
        k.reset()
      }
    })
    t.setDaemon(true)
    t.start()
  }

  def main(args: Array[String]): Unit = {
    val useGA = args.contains("--ga")
    val useBSP = args.contains("--bsp")
    val load = args.contains("--load")

    val dataPath = Paths.get("data")
    val data = Data.parse(dataPath)

    val savePath = Paths.get("save.bson")
    val state =
      if (Files.exists(savePath) && load) {
        val start = System.nanoTime()
        val json = Bson.decode(Files.newInputStream(savePath))
        println(f"Parse took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        val start2 = System.nanoTime()
        val state = Serialization.load(data, json)
        println(f"Load took ${(System.nanoTime() - start2) / 1e6}%.1f ms")
        state
      } else {
        implicit val random: Random = new Random(12367)
        val gen = WorldGen(data)
        val state = if (useGA) gen.generateWorldGA else if (useBSP) gen.generateWorldBSP else gen.generateWorld
        state.refresh()
        //val start = System.nanoTime()
        //Bson.encode(Serialization.save(state), Files.newOutputStream(savePath))
        //println(f"Save took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        state
      }

    state.recalculateFOV()
    val display: Display = new GLFWDisplay
    display.init()
    display.update(state)

    onFileChanged(dataPath) { _ =>
      display.postAction(Action.ReloadData(Data.parse(dataPath)))
    }

    while (display.running) {
      val action = display.waitForAction
      state.receive(action)
      display.update(state)
    }
  }
}
