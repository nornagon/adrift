package adrift

import java.nio.file.{FileSystems, Files, Path, StandardWatchEventKinds, WatchEvent, WatchKey}
import java.nio.file.attribute.BasicFileAttributes

import com.sun.nio.file.SensitivityWatchEventModifier

object FileWatcher {
  def onFileChanged(path: Path)(f: Path => Unit): Unit = {
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
        import scala.jdk.CollectionConverters._
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
}
