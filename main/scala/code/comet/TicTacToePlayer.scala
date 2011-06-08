package code.comet

import net.liftweb._
import common._
import http._
import js._
import util._
import Helpers._
import actor._
import JE._

import scala.collection.mutable._
import scala.xml._
import util._

class TicTacToePlayer extends CometActor {
  var boardStatus: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer(ArrayBuffer(" "," "," "),ArrayBuffer(" "," "," "),ArrayBuffer(" "," "," "))
  var players = HashMap("X" -> "Nobody", "O" -> "Nobody")
  var symbol = "NONE"
  var gameInfo = ""
  var winSequence:ArrayBuffer[Tuple2[Int,Int]] = new ArrayBuffer[Tuple2[Int, Int]]
  val playerId = new scala.util.Random().nextInt(50000)
  
  override def lowPriority = {
    case TicTacToeServer.BoardChange(board: ArrayBuffer[ArrayBuffer[String]]) => {
      gameInfo = ""
      boardStatus = board
      reRender()
    }
    case TicTacToeServer.Success(true) =>
      gameInfo = ""
      println("Success n such")
    case TicTacToeServer.PlayerChange(changedSymbol: String, changedPlayerId: String) =>
      gameInfo = ""
      players(changedSymbol) = changedPlayerId.toString()
      if (playerId.toString() == changedPlayerId) symbol = changedSymbol
      reRender()
    case TicTacToeServer.TTTStatus(status: String) =>
      gameInfo = status
      reRender()
    case TicTacToeServer.Win(sequence) =>
      winSequence = sequence
      reRender()
    case TicTacToeServer.NewGame(status) =>
      if (status == "NEW") {
        winSequence = new ArrayBuffer[Tuple2[Int, Int]]
        reRender()
      }
    case _ => println("Other lp")
  }
  
  override def localSetup = {
    TicTacToeServer ! TicTacToeServer.RegisterPlayer(this, playerId.toString())
  }
  
  def render = {
    val xButton = <button id="xButton" value="X" type="button">{S.?("X")}</button> %
    	("onclick" -> SHtml.ajaxCall(ValById("xButton"), choosePlayer _ )._2)
    val oButton = <button id="oButton" value="O" type="button">{S.?("O")}</button> %
    	("onclick" -> SHtml.ajaxCall(ValById("oButton"), choosePlayer _ )._2)
    def renderSquare(row: Int, col: Int) = {
      val square = boardStatus(row)(col)
      var style = "cursor:pointer;"
      if (!winSequence.isEmpty && winSequence.contains((row, col))) {
        println()
        style = String.format("%s%s", style, "background-color:red;")
      }
      if (square == TicTacToeServer.EMPTY) {
        <td class="tttSquare" style={style}>&nbsp;</td> %
          ("onclick" -> SHtml.ajaxCall(JsArray(Num(row), Num(col)), move _ )._2)
      } else {
    	  <td class="tttSquare" style={style}>{boardStatus(row)(col)}</td>
      }
    }
    def resetGame = {
      if(!winSequence.isEmpty) {
        <button id="resetButton" value="reset" type="button">{S.?("New Game")}</button> %
        	("onclick" -> SHtml.ajaxCall(ValById("resetButton"), newGame _)._2)
      }
    }
    def symbolSelect = {
      if (symbol == "NONE") {
        <span>Choose Your Symbol</span>
		<span>{xButton}</span>
        <span>{oButton}</span>
      } 
    }
    def symbolDisplay = {
      if (symbol != "NONE") {
        <span class="tttPlayer">Playing as {symbol}</span>
      }
    }
    def boardView: NodeSeq = {
      (<table class="tttBoard">
      { for (row <- 0 until boardStatus.length) yield
        (<tr>
          { for (col <- 0 until boardStatus(row).length) yield
            ({renderSquare(row, col)})
          }
        </tr>)
      }
      </table>)
    }
  <div class="tttGameInfo">{gameInfo}</div>
  <div>{resetGame}</div>
  <div>Your player id is: <span id="player_id">{playerId}</span></div>
  <div>{symbolSelect}</div>
  <div>{symbolDisplay}</div>
  <ul class="tttPlayerList">
	  <li>X : {players("X")}</li>
	  <li>O : {players("O")}</li>
  </ul>
  <div>{boardView}</div>
  }
  def move(coords: String): JsCmd = {
    val splitCoords = coords.split(",")
    TicTacToeServer ! TicTacToeServer.MakeMove(playerId.toString(), Integer.parseInt(splitCoords(0)), Integer.parseInt(splitCoords(1)))
    reRender()
  }
  def choosePlayer(s:String): JsCmd = {
    TicTacToeServer ! TicTacToeServer.ChooseSymbol(playerId.toString(), s)
    reRender()
  }
  def newGame(s:String): JsCmd = {
    TicTacToeServer ! TicTacToeServer.NewGame(playerId.toString())
    reRender()
  }
}
