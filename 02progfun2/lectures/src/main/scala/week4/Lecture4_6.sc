import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Lecture4_6 {
  val socket = Socket()
  val packet: Future[Array[Byte]] = socket.readFromMemory
  // Option 3 : FlatMaps
  val confirmation: Future[Array[Byte]] =
    packet.flatMap(p => socket.sendToSafe(p))

  trait SocketTrait {
    def readFromMemory: Future[Array[Byte]]

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }

  class Socket extends SocketTrait {
    def readFromMemory: Future[Array[Byte]] = Future((1 :: 2 :: 3 :: Nil).toArray[Byte])

    def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] =
      sendToEurope(packet) recoverWith {
        case europeError => sendToUsa(packet) recover {
          case usaError => usaError.getMessage.toArray[Byte]
        }
      }

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] =
      Http(new URL("mail.server.eu"), new Request(packet))
        .filter(response => response.isOK)
        .map(response => response.toByteArray)

    def sendToUsa(packet: Array[Byte]): Future[Array[Byte]] =
      Http(new URL("mail.server.usa"), new Request(packet))
        .filter(response => response.isOK)
        .map(response => response.toByteArray)
  }

  // Example
  class URL(val url: String) {}

  class Request(val packet: Array[Byte]) {}

  class Response {
    def toByteArray = List(1, 2, 3).toArray[Byte]

    def isOK = true
  }

  object Socket {
    def apply[T](expr: T) = new Socket()
  }

  object Http {
    def apply(url: URL, req: Request): Future[Response] = Future(new Response())
  }

}