package retro.core

import spinal.core._
import spinal.lib.misc.database.Database
import spinal.lib.misc.plugin._

/** Empty-host toplevel, the VexiiRiscv pattern: the core is nothing but its plugin list.
  * Every fiber the plugins start runs with `database` in scope, so `Database.blocking` keys
  * are visible to all of them. */
class RetroCore extends Component {
  val database = new Database
  val host = database on (new PluginHost)
}

object RetroCore {
  def apply(plugins: scala.collection.Seq[Hostable]): RetroCore = {
    val core = new RetroCore
    core.host.asHostOf(plugins)
    core
  }
}
