object Lecture4_4 {

  val adventure = Adventure()
  val coins = adventure.collectCoins()
  val treasure = adventure.buyTreasure(coins)
  val socket = Socket()
  // Reading from memory has a high latency
  val packet = socket.readFromMemory()
  // Sending packet to Europe and back has a HUGE latency
  val confirm = socket.sendToEurope(packet)

  // Sample adventure game
  trait Coin

  trait Treasure extends Coin

  // ---------------------------------------------------------------------- //

  trait AdventureTrait {
    def collectCoins(): List[Coin]

    def buyTreasure(coins: List[Coin]): Treasure
  }

  // Make it a network script
  trait SocketTrait {
    def readFromMemory(): Array[Byte]

    def sendToEurope(packet: Array[Byte]): Array[Byte]
  }

  class Adventure extends AdventureTrait {
    def collectCoins(): List[Coin] = ???

    def buyTreasure(coins: List[Coin]): Treasure = ???
  }

  class Socket extends SocketTrait {
    def readFromMemory(): Array[Byte] = ???

    def sendToEurope(packet: Array[Byte]): Array[Byte] = ???
  }

  object Adventure {
    def apply[T](expr: T) = new Adventure()
  }

  object Socket {
    def apply[T](expr: T) = new Socket()
  }

}