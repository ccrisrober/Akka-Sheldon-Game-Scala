// Copyright (c) 2016, maldicion069 (Cristian Rodríguez) <ccrisrober@gmail.con>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.package com.example

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.routing.RoundRobinRouter
import scala.util.Random
import scala.collection.mutable.ListBuffer

sealed trait DASGame

object Positions {
  sealed trait EnumVal
  case object Piedra extends EnumVal
  case object Papel extends EnumVal
  case object Tijeras extends EnumVal
  case object Lagarto extends EnumVal
  case object Spock extends EnumVal
  val posibleOpts = Seq(Piedra, Papel, Tijeras, Lagarto, Spock)
}

case class Hand(id: Int) extends DASGame
case class Result(id: Int, value: Int/*, value: Positions*/) extends DASGame
case class RoundResult(results: List[Result], ref: ActorRef) extends DASGame

case object PlayGame extends DASGame
case object FinishGame extends DASGame

    
class Player extends Actor {
    val r: Random = new Random()
      
    def readValue(): Int = r.nextInt(5) //Positions = Positions.Piedra //.values()(r.nextInt(5))
    def receive = {
        case Hand(id) =>
            println(id)
            sender ! Result(id, readValue())
    }
}

class Game {

    def startGame(players: Int) {
        val system = ActorSystem("PiSystem")
 
        // create the result listener, which will print the result and shutdown the system
        val listener = system.actorOf(Props[Listener], name = "listener")
 
        // create the master
        val master = system.actorOf(Props(new Master(players, listener)), name = "master")
     
        // start the calculation
        master ! PlayGame
    }
}
object Listener {
    private var vueltas = 10
}
class Listener extends Actor {
    def receive = {
        case RoundResult(results, ref) => 
            println ("Recibo resultados")
            
            val rocks = results.filter(_.value == 0)
            val papers = results.filter(_.value == 1)
            val scissors = results.filter(_.value == 2)
            val alligators = results.filter(_.value == 3)
            val spocks = results.filter(_.value == 4)
            
            val r = new ListBuffer[Int]()
            r += rocks.size
            r += papers.size
            r += scissors.size
            r += alligators.size
            r += spocks.size
            
            val count = r.toList.filter(_ > 0)
            
            var winPiedra = false
            var winPapel = false
            var winTijeras = false
            var winLagarto = false
            var winSpock = false
            
            if (count == 2) {
                var p1 = -1   
                var p2 = -1  
                if (!rocks.isEmpty) {
                    if (p1 == -1) {
                        p1 = 0
                    } else if (p2 == -1) {
                        p2 = 0
                    }
                }
                if (!papers.isEmpty) {
                    if (p1 == -1) {
                        p1 = 1
                    } else if (p2 == -1) {
                        p2 = 1
                    }
                }
                if (!scissors.isEmpty) {
                    if (p1 == -1) {
                        p1 = 2
                    } else if (p2 == -1) {
                        p2 = 2
                    }
                }
                if (!alligators.isEmpty) {
                    if (p1 == -1) {
                        p1 = 3
                    } else if (p2 == -1) {
                        p2 = 3
                    }
                }
                if (!spocks.isEmpty) {
                    if (p1 == -1) {
                        p1 = 4
                    } else if (p2 == -1) {
                        p2 = 4
                    }
                }
                
                if (p1 == p2) {
                    println ("EMPATE")
                } else {
                    println (r)
                    if (p1 == 0) {
                        if (p2 == 2) {
                            winPiedra = true
                        } else if (p2 == 1) {
                            winPapel = true
                        } else if (p2 == 3) {
                            winPiedra = true
                        } else if (p2 == 4) {
                            winSpock = true
                        }
                    }
                }
                Listener.vueltas = Listener.vueltas - 1
            } else {
                println ("EMPATE TOTAL")
            }
            
            if (Listener.vueltas > 0) {
                ref ! PlayGame
            } else {
                ref ! FinishGame
                context.stop(self)
            }
            
    }
}

class Master(var numPlayers: Int, var listener: ActorRef) extends Actor {

    protected var players: List[Player] = List()
    
    protected var results: List[Result] = List()
    
    protected val workerRouter = context.actorOf(Props[Player].withRouter(RoundRobinRouter(numPlayers)), name = "workerRouter")
    
    def receive = {
        case PlayGame =>
            //players = List()
            for (i ← 0 until numPlayers) workerRouter ! Hand(i)
        case Result(id, value) =>
            println(id + " " + value)
            this.results = this.results :+ Result(id, value)
            
            println(this.results.size)
            if(this.results.size == numPlayers) {
                listener ! RoundResult(results, self)
            }
        case FinishGame => 
    }

}

object HelloAkkaScala extends App {
    new Game().startGame(2)
}