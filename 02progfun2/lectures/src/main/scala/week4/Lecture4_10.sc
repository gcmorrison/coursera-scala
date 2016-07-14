import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Lecture4_10 {

  val socket = Socket()
  val confirmation: Future[Array[Byte]] = for {
    packet <- socket.readFromMemory
    confirmation <- socket.sendToSafe(packet)
  } yield confirmation

  trait SocketTrait {
    def readFromMemory: Future[Array[Byte]]

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }

  class Socket extends SocketTrait {
    def readFromMemory: Future[Array[Byte]] = Future((1 :: 2 :: 3 :: Nil).toArray[Byte])

    def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] =
      sendToEurope(packet) fallbackTo {
        sendToUsa(packet)
      } recover {
        case europeError => europeError.getMessage.toArray[Byte]
      }

    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] =
      Http(new URL("mail.server.eu"), new Request(packet))
        .filter(response => response.isOK)
        .map(response => response.toByteArray)

    def sendToUsa(packet: Array[Byte]): Future[Array[Byte]] =
      Http(new URL("mail.server.usa"), new Request(packet))
        .filter(response => response.isOK)
        .map(response => response.toByteArray)

    def fallbackTo[T](that: => Future[T]): Future[T] = {
      this recoverWith {
        case _ => that recoverWith { case _ => this }
      }
    }

    // FoldLeft retry mechanism
    def retryLeft[T](noTimes: Int)(block: => Future[T]): Future[T] = {
      val ns = (1 to noTimes).toList
      val attempts = ns.map(_ => () => block)
      val failed = Future.failed(new Exception("boom"))
      val result = attempts.foldLeft(failed)((a, block) => a recoverWith {
        block()
      })
      result
    }

    // FoldRight retry mechanism
    def retryRight[T](noTimes: Int)(block: => Future[T]): Future[T] = {
      val ns = (1 to noTimes).toList
      val attempts = ns.map(_ => () => block)
      val failed = Future.failed(new Exception("boom"))
      val result = attempts.foldRight(() => failed)((a, block) => () => {
        block() fallbackTo {
          a()
        }
      })
      result()
    }
  }

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