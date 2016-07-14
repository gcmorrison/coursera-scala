import scala.concurrent.Future
import scala.util.{Failure, Success}

object Lecture4_5 {

  val socket = Socket()
  val packet: Future[Array[Byte]] = socket.readFromMemory
  // Option 1 : Bad; should return Future[Array[Byte]], but now returns Unit
  val confirmation1: Future[Array[Byte]] = packet.onComplete {
    case Success(p) => socket.sendToEurope(p)
    case Failure(t) => ???
  }

  trait SocketTrait {
    def readFromMemory: Future[Array[Byte]]

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }

  class Socket extends SocketTrait {
    def readFromMemory: Future[Array[Byte]] = ???

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] = ???
  }

  object Socket {
    def apply[T](expr: T) = new Socket()
  }

  // Option 2 : Meh; returns the right type, but it's very nested
  packet.onComplete {
    case Success(p) => {
      val confirmation = socket.sendToEurope(p)
    }
    case Failure(t) => ???
  }

  // Option 3 : Better
  // Covered in next lecture
}