package com.neiloconnor.tankyspanky

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import com.neiloconnor.tankyspanky.BotActor.{CreateTank, Move, Test}

import scala.concurrent.duration._

object TankySpanky extends App{

  val argsMap: Map[String,String] = parse(args)

  val system = ActorSystem("TankySpanky")
  val remote = new InetSocketAddress(argsMap.getOrElse("ip", "localhost"), argsMap.getOrElse("port", "8052").toInt)
  val bot = system.actorOf(BotActor.props())
  val tcpClient = system.actorOf(TcpClientActor.props(remote, bot))

  Thread.sleep(500)  // wait for tcp connection

  bot ! CreateTank

  import system.dispatcher
  system.scheduler.schedule(1000 milliseconds,500 milliseconds, bot, Move)

  def parse(args: Array[String]): Map[String,String] = {
    args.headOption match {
      case Some(arg) =>{
        arg match {
          case arg if arg.startsWith("-ip") =>
            Map("ip" -> arg.split("=")(1)) ++ parse(args.tail)
          case arg if arg.startsWith("-name") =>
            Map("name" -> arg.split("=")(1)) ++ parse(args.tail)
          case arg if arg.startsWith("-port") =>
            Map("port" -> arg.split("=")(1)) ++ parse(args.tail)
        }
      }
      case None => Map.empty
    }
  }

}
