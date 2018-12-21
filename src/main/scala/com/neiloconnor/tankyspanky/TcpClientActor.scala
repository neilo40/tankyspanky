package com.neiloconnor.tankyspanky

import java.net.InetSocketAddress
import TankMessage.toTankMessage
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.util.ByteString
import BotActor.RegisterTcpClient

object TcpClientActor {
  def props(remote: InetSocketAddress, listener: ActorRef): Props = Props(new TcpClientActor(remote, listener))
}

class TcpClientActor(remote: InetSocketAddress, listener: ActorRef) extends Actor with ActorLogging {
  import context.system

  IO(Tcp) ! Connect(remote)

  override def receive: Receive = {
    case CommandFailed(_: Connect) =>
      context stop self

    case c@Connected(remote, local) ⇒
      log.info("connected to {} ", remote.getAddress)
      val connection = sender()


      connection ! Register(self)
      listener ! RegisterTcpClient
      log.info("Registered")

      context become {
        case data: ByteString ⇒
          connection ! Write(data)
        case CommandFailed(w: Write) ⇒
          // O/S buffer was full
        case Received(data) ⇒
          toTankMessage(data).foreach {
            listener !
          }
        case "close" ⇒
          connection ! Close
        case _: ConnectionClosed ⇒
          log.info("stopping")

          context stop self
      }
  }


}
