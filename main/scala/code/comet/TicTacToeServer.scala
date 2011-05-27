package code.comet

import net.liftweb._
import common._
import http._
import actor._

import scala.collection._
import mutable._
/**
 * Singleton providing tic tac toe board management to all clients.
 * Thread-safe actor.
 */
object TicTacToeServer extends CometActor {
  case class RegisterPlayer(player: CometActor, playerId: String)
  case class ChooseSymbol(playerId:String, symbol:String)
  case class MakeMove(playerId: String, xCoord: Int, yCoord: Int)
  case class BoardChange(board: Array[Array[String]])
  case class PlayerChange(symbol: String, playerId: String)
  case class TTTStatus(status: String)
  case class Success(success: Boolean)
  
  val EMPTY = " "
  val X = "X"
  val O = "O"
  private var board = Array(Array(EMPTY,EMPTY,EMPTY), Array(EMPTY,EMPTY,EMPTY), Array(EMPTY,EMPTY,EMPTY))
  private var activePlayers = new HashMap[String, (String, CometActor)]
  private var availPlayers = new HashMap[String, CometActor]
  private var nextTurn = X
  
  def notifyPlayers(board: Array[Array[String]]) = {
    for (player <- availPlayers.values) {
      player ! BoardChange(board)
    }
  }
  
  def notifyPlayers(playerId: String, symbol: String) = {
    for (player <- availPlayers.values) {
      player ! PlayerChange(symbol, playerId)
    }
  }
  
  def notifyPlayers(status: String) = {
    for (player <- availPlayers.values) {
      player ! TTTStatus(status)
    }
  }
  
  def notifyPlayer(playerId: String, status: String) = {
    availPlayers(playerId) ! TTTStatus(status)
  }
  
  def getPlayerSymbol(playerId: String) = {
    var symbol:Box[String] = Empty
    if (activePlayers(X)._1 == playerId) symbol = Full(X)
    if (activePlayers(O)._1 == playerId) symbol = Full(O)
    symbol
  }
  
  def changeTurn() = {
    if (nextTurn == X) nextTurn = O
    else if (nextTurn == O) nextTurn = X
    notifyPlayers(nextTurn + "'s move!")
  }
  
  def checkEndGame(): Boolean = {
    //Check if all in row, all in col, or a diag is set.
    for (i <- 0 until 2) {
      //Check row.
      if (board(i)(0) != EMPTY && board(i)(0) == board(i)(1) && board(i)(1) == board(i)(2)) {
        return true;
      }
      //Check col.
      if (board(0)(i) != EMPTY && board(0)(i) == board(1)(i) && board(1)(i) == board(2)(i)) {
        return true;
      }
    }
    //Check diag.
    if (board(0)(0) != EMPTY && board(0)(0) == board(1)(1) && board(1)(1) == board(2)(2)) {
      return true;
    }
    return false;
  }
  
  override def lowPriority = {
    case RegisterPlayer(player: CometActor, playerId: String) =>
      availPlayers(playerId) = player
      reply(Success(true))
    case ChooseSymbol(playerId: String, symbol: String) =>
      //Can only choose symbol if there are at least two available players.
      if (availPlayers.size < 2) {
        notifyPlayer(playerId, "Can only choose symbol if there are at least two available players.")
      }
      //Check player doesn't already exist, symbol is valid, and player is registered.
      else if (!activePlayers.contains(symbol) && (symbol == X || symbol == O) && availPlayers.contains(playerId)) {
        val player = availPlayers(playerId)
        activePlayers(symbol) = (playerId, player)
        println("Choosing a symbol.")
        notifyPlayers(playerId, symbol)
        //Check for two players.
        if (activePlayers.contains(X) && activePlayers.contains(O)) {
          //We have two players.  Inform of move.
          notifyPlayers("Game on, X moves first!")
        }
      }
    case MakeMove(playerId: String, xCoord: Int, yCoord: Int) =>
      //Can't play unless two players exist already.
      if (!activePlayers.contains(X) || !activePlayers.contains(O)) {
        notifyPlayer(playerId, "Can only make move if there are players registered for both symbols.")  
      } else {
        //Check that correct turn is moving.
        if (playerId == activePlayers(nextTurn)._1) {
          //Update board model and notifyPlayers
          val logger = Logger(classOf[TicTacToePlayer])
          val symbol = getPlayerSymbol(playerId)
          if (!symbol.isEmpty) {
            board(xCoord)(yCoord) = symbol.open_! 
          }
          notifyPlayers(board)
          if(checkEndGame()) {
            notifyPlayers("Game over. " + symbol.open_! + " wins!")
          }
          else {
            changeTurn()
          }
        } else {
          //Wrong player is trying to move.  Tell them it's not their turn.
          //TODO: Check and make sure person trying to move is playing at least.
          notifyPlayer(playerId, "It's not your turn to move.  It's " + nextTurn + "'s move.")
        }
      }
    case _ => println("Other lp")
  }
  
  def render = {
    <div>The Server</div>
  }
}
