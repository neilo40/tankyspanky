package com.neiloconnor.tankyspanky

import java.util.concurrent.TimeUnit

import TankMessage._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import collection.JavaConverters._
import com.google.common.cache.{Cache, CacheBuilder}

object Mode extends Enumeration {
  type Mode = Value
  val Kill, Bank, Ammo, Health, Snitch = Value
}

object BotActor {
  def props(): Props = Props(new BotActor())

  case object Test
  case object CreateTank
  case object RegisterTcpClient
  case object Move
}

class BotActor() extends Actor with ActorLogging{
  import BotActor._
  import Mode._

  private val name = "TankySpanky"
  private var tcpClient: Option[ActorRef] = None
  private var myTank: UpdatePayload = _
  private var timeRemaining = 0
  private var mode = Kill
  private var pointsToBank = false
  private var huntCount = 0 // how many times have we searched in vain?
  private var snitch: Option[UpdatePayload] = None
  private var beastMode = false
  private val enemiesCache: Cache[Integer, UpdatePayload] = CacheBuilder.newBuilder()
    .expireAfterWrite(2, TimeUnit.SECONDS)
    .build[Integer, UpdatePayload]
  private val ammoCache: Cache[Integer, UpdatePayload] = CacheBuilder.newBuilder()
    .expireAfterWrite(2, TimeUnit.SECONDS)
    .build[Integer, UpdatePayload]
  private val healthCache: Cache[Integer, UpdatePayload] = CacheBuilder.newBuilder()
    .expireAfterWrite(2, TimeUnit.SECONDS)
    .build[Integer, UpdatePayload]

  private def getHeading(x1: Float, y1: Float, x2: Float, y2: Float) = {
    var heading = math.atan2(y2 - y1, x2 - x1).asInstanceOf[Float]
    heading = radianToDegree(heading).toFloat
    heading = (heading - 360) % 360
    math.abs(heading)
  }

  private def radianToDegree(angle: Double) = angle * (180.0 / Math.PI)

  private def calculateDistance(target: UpdatePayload) = {
    val ownX = myTank.X
    val ownY = myTank.Y
    val otherX = target.X
    val otherY = target.Y
    val headingX = otherX - ownX
    val headingY = otherY - ownY
    math.sqrt((headingX * headingX) + (headingY * headingY)).asInstanceOf[Float]
  }

  private def getNearest(targets: List[UpdatePayload]) =
    if (targets.isEmpty) myTank else targets.minBy(calculateDistance)

  override def receive: Receive = {
    case Move =>
      if (myTank == null)
        log.info("Not spawned yet")
      else if (huntCount > 5){
        log.info("Heading to the death zone for a better look")
        val centerHeading = getHeading(myTank.X, myTank.Y, 0, 0).toInt
        tcpClient.get ! toByteString(TankMessage("turnToHeading", Some(AmountPayload(centerHeading))))
        tcpClient.get ! toByteString(TankMessage("moveForwardDistance", Some(AmountPayload(20))))
        huntCount = 0
      } else {
        mode match {
          case Kill =>
            log.info("Kill")
            if (enemiesCache.asMap().asScala.values.isEmpty) {
              tcpClient.get ! toByteString(TankMessage("turnTurretToHeading", Some(AmountPayload(myTank.TurretHeading.toInt + 90))))
              huntCount += 1
            } else {
              val nearestEnemy = if (beastMode) {
                snitch.getOrElse(getNearest(enemiesCache.asMap().asScala.values.toList))
              } else {
                getNearest(enemiesCache.asMap().asScala.values.toList)
              }
              log.info("Nearest enemy: "+ nearestEnemy)
              val headingToTank = getHeading(myTank.X, myTank.Y, nearestEnemy.X, nearestEnemy.Y).toInt
              val distanceToEnemy = calculateDistance(nearestEnemy)
              val distanceToMove = if (distanceToEnemy < 10) 0 else 5
              tcpClient.get ! toByteString(TankMessage("turnToHeading", Some(AmountPayload(headingToTank))))
              tcpClient.get ! toByteString(TankMessage("turnTurretToHeading", Some(AmountPayload(headingToTank))))
              tcpClient.get ! toByteString(TankMessage("moveForwardDistance", Some(AmountPayload(distanceToMove))))
              if (distanceToEnemy < 30)
                tcpClient.get ! toByteString(TankMessage("fire"))
              huntCount = 0
            }
          case Snitch =>
            log.info("Snitch")
          case Ammo =>
            log.info("looking for Ammo")
            log.info("I am: " + myTank)
            log.info("Ammo: " + ammoCache.asMap())
            if (ammoCache.asMap().asScala.values.isEmpty) {
              tcpClient.get ! toByteString(TankMessage("turnTurretToHeading", Some(AmountPayload(myTank.TurretHeading.toInt + 90))))
              huntCount += 1
            } else {
              val nearestAmmo = getNearest(ammoCache.asMap().asScala.values.toList)
              val headingToAmmo = getHeading(myTank.X, myTank.Y, nearestAmmo.X, nearestAmmo.Y).toInt
              tcpClient.get ! toByteString(TankMessage("turnToHeading", Some(AmountPayload(headingToAmmo))))
              tcpClient.get ! toByteString(TankMessage("moveForwardDistance", Some(AmountPayload(10))))
              huntCount = 0
            }
          case Health =>
            log.info("looking for Health")
            log.info("I am: " + myTank)
            log.info("Health: " + healthCache.asMap())
            if (healthCache.asMap().asScala.values.isEmpty) {
              tcpClient.get ! toByteString(TankMessage("turnTurretToHeading", Some(AmountPayload(myTank.TurretHeading.toInt + 90))))
              huntCount += 1
            } else {
              val nearestHealth = getNearest(healthCache.asMap().asScala.values.toList)
              val headingToHealth = getHeading(myTank.X, myTank.Y, nearestHealth.X, nearestHealth.Y).toInt
              tcpClient.get ! toByteString(TankMessage("turnToHeading", Some(AmountPayload(headingToHealth))))
              tcpClient.get ! toByteString(TankMessage("moveForwardDistance", Some(AmountPayload(10))))
              huntCount = 0
            }
          case Bank =>
            log.info("Banking")
            // go to nearest goal
            val goalTarget = if (myTank.Y > 0) (0, 100) else (0, -100)
            val headingToGoal = getHeading(myTank.X, myTank.Y, goalTarget._1, goalTarget._2).toInt
            tcpClient.get ! toByteString(TankMessage("turnToHeading", Some(AmountPayload(headingToGoal))))
            tcpClient.get ! toByteString(TankMessage("moveForwardDistance", Some(AmountPayload(10))))
          case _ =>
            log.info("Unknown mode")
        }
      }
    case RegisterTcpClient =>
      tcpClient = Some(sender())
    case CreateTank =>
      tcpClient.get ! toByteString(TankMessage("createTank", Some(CreateTankPayload(name))))
    case Test =>
      log.info("Sending test")
      tcpClient.get ! toByteString(TankMessage("test"))
    case msg: TankMessage =>
      msg.messageType match {
        case "gameTimeUpdate" =>
          timeRemaining = msg.oPayload.get.asInstanceOf[TimePayload].Time
        case "objectUpdate" =>
          val p = msg.oPayload.get.asInstanceOf[UpdatePayload]
          p.Type match {
            case "Tank" =>
              p.Name match {
                case `name` =>
                  mode = if (p.Health < 3) Health else if (p.Ammo == 0 && mode != Bank) Ammo else mode
                  myTank = p
                case _ =>
                  enemiesCache.put(p.Id, p)
              }
            case "HealthPickup" =>
              healthCache.put(p.Id, p)
            case "AmmoPickup" =>
              ammoCache.put(p.Id, p)
            case "Snitch" =>
              snitch = Some(p)
            case _ =>
              log.info("Unknown payload received: " + msg.toString)
          }
        case "destroyed" =>
          mode = Kill
        case "kill" =>
          mode = Bank
          pointsToBank = true
        case "enteredGoal" =>
          mode = Kill
          pointsToBank = false
        case "ammoPickup" =>
          mode = if (pointsToBank) Bank else Kill
        case "healthPickup" =>
          mode = if (pointsToBank) Bank else Kill
        //case "snitchAppeared" =>
        //  mode = Snitch
        case "snitchPickup" =>
          log.info("Snitch picked up.  Beast mode activate!!1!")
          val p = msg.oPayload.get.asInstanceOf[IDPayload]
          mode = if (p.Id == myTank.Id) Bank else mode
          beastMode = true
        case _ => log.info("got some other message: " + msg.messageType + "," + msg.oPayload.getOrElse("no body").toString)
      }
  }

}
