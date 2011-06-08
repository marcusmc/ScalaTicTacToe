package code.comet

import net.liftweb._
import common._
import http._
import actor._

import scala.collection.mutable._
/**
 * Singleton providing tic tac toe board management to all clients.
 * Thread-safe actor.
 */
object TicTacToeServer extends CometActor {
  abstract class TTTMessage
  case class RegisterPlayer(player: CometActor, playerId: String) extends TTTMessage
  case class ChooseSymbol(playerId:String, symbol:String) extends TTTMessage
  case class MakeMove(playerId: String, xCoord: Int, yCoord: Int) extends TTTMessage
  case class BoardChange(board: ArrayBuffer[ArrayBuffer[String]]) extends TTTMessage
  case class PlayerChange(symbol: String, playerId: String) extends TTTMessage
  case class TTTStatus(status: String) extends TTTMessage
  case class Success(success: Boolean) extends TTTMessage
  case class NewGame(playerId: String) extends TTTMessage
  
  abstract class TTTState extends TTTMessage
  case class Win(winRows: ArrayBuffer[Tuple2[Int, Int]]) extends TTTState
  case class Cat() extends TTTState
  case class Continue() extends TTTState
  
  val EMPTY = " "
  val X = "X"
  val O = "O"
  private var board = ArrayBuffer(ArrayBuffer(EMPTY,EMPTY,EMPTY), ArrayBuffer(EMPTY,EMPTY,EMPTY), ArrayBuffer(EMPTY,EMPTY,EMPTY))
  private var activePlayers = new HashMap[String, (String, CometActor)]
  private var availPlayers = new HashMap[String, CometActor]
  private var nextTurn = X
  
  def notifyPlayers(message: TTTMessage) = {
    for (player <- availPlayers.values) {
      player ! message
    }
  }
  
  def resetBoard() = {
    board = board map (row => row map (cell => EMPTY))
  }
  
  def notifyPlayer(playerId: String, message: TTTMessage) = {
    availPlayers(playerId) ! message 
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
    notifyPlayers(TTTStatus(nextTurn + "'s move!"))
  }
  
  def getBoardAt(indices: Tuple2[Int, Int]): String = {
    board(indices._1)(indices._2)
  }
  
  def checkSquares(squares: ArrayBuffer[Tuple2[Int, Int]]): Boolean = {
     val checkSymbol = getBoardAt(squares(0))
     if (checkSymbol != EMPTY && (squares forall (indices => getBoardAt(indices) == checkSymbol))) {
       return true
     } else {
       return false
     }
  }
  
  def checkState(): TTTState = {
    //Check if all in row, all in col, or a diag is set.
    //Generate win conditions
    notifyPlayers(TTTStatus("checking state"))
    val winConditions:ArrayBuffer[ArrayBuffer[Tuple2[Int, Int]]] = new ArrayBuffer[ArrayBuffer[Tuple2[Int, Int]]]
    for (i <- 0 until 3) {
      winConditions += ArrayBuffer((0,i),(1,i),(2,i))
      winConditions += ArrayBuffer((i,0),(i,1),(i,2))
    }
    //Add diag.
    winConditions += ArrayBuffer((0,0),(1,1),(2,2))
    winConditions += ArrayBuffer((2,0),(1,1),(0,2))
    for (sequence <- winConditions) {
      if (checkSquares(sequence)) {
        return Win(sequence)
      }
    }
    if (board forall(row => row forall (cell => cell != EMPTY))) {
      return Cat();
    }
    return Continue();
  }
  
  override def lowPriority = {
    case RegisterPlayer(player: CometActor, playerId: String) =>
      availPlayers(playerId) = player
      reply(Success(true))
    case ChooseSymbol(playerId: String, symbol: String) =>
      //Can only choose symbol if there are at least two available players.
      if (availPlayers.size < 2) {
        notifyPlayer(playerId, TTTStatus("Can only choose symbol if there are at least two available players."))
      }
      //Check player doesn't already exist, symbol is valid, and player is registered.
      else if (!activePlayers.contains(symbol) && (symbol == X || symbol == O) && availPlayers.contains(playerId)) {
        val player = availPlayers(playerId)
        activePlayers(symbol) = (playerId, player)
        notifyPlayers(PlayerChange(symbol, playerId))
        //Check for two players.
        if (activePlayers.contains(X) && activePlayers.contains(O)) {
          //We have two players.  Inform of move.
          notifyPlayers(TTTStatus("Game on, X moves first!"))
        }
      }
    case MakeMove(playerId: String, xCoord: Int, yCoord: Int) =>
      //Can't play unless two players exist already.
      if (!activePlayers.contains(X) || !activePlayers.contains(O)) {
        notifyPlayer(playerId, TTTStatus("Can only make move if there are players registered for both symbols."))  
      } else {
        //Check that correct turn is moving.
        if (playerId == activePlayers(nextTurn)._1) {
          //Update board model and notifyPlayers
          val symbol = getPlayerSymbol(playerId)
          if (!symbol.isEmpty) {
            board(xCoord)(yCoord) = symbol.open_! 
          }
          notifyPlayers(BoardChange(board))
          val endGame = checkState
          endGame match {
            case Win(sequence) =>
              notifyPlayers(TTTStatus("Game over. " + symbol.open_! + " wins!"))
              notifyPlayers(Win(sequence))
              nextTurn = EMPTY
            case Cat() =>
              notifyPlayers(TTTStatus("Game over. No one wins...well, except the cat."))
            case Continue() =>
              changeTurn()
          }
        } else {
          //Wrong player is trying to move.  Tell them it's not their turn.
          //TODO: Check and make sure person trying to move is playing at least.
          notifyPlayer(playerId, TTTStatus("It's not your turn to move.  It's " + nextTurn + "'s move."))
        }
      }
    case NewGame(playerId: String) =>
      if (nextTurn == EMPTY) {
      	resetBoard
        nextTurn = X
      	notifyPlayers(TTTStatus("New game! X moves first!"))
        notifyPlayers(NewGame("NEW"))
        notifyPlayers(BoardChange(board))
     }
    case _ => println("Unidentified low-priority message.")
  }
  
  def render = {
    <div>The Server</div>
  }
}
