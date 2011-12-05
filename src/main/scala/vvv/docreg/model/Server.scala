package vvv.docreg.model

class Server(val description: String, val name: String, val address: String) {
}

object Server {
  lazy val all: List[Server] = {
    new Server("Santa Clara", "boromir", "10.15.168.22") ::
    new Server("NZ", "shelob", "10.16.9.179") ::
    new Server("RTP", "ugluk", "10.15.32.210") ::
    new Server("UK", "balrog", "10.11.65.21") ::
    new Server("Singapore", "treebeard", "10.11.48.28") ::
    new Server("Slovenia", "beregond", "192.168.180.30") ::
    new Server("India", "eomer", "192.168.250.51") :: Nil
  }
}