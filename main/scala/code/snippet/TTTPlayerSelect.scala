package code.snippet

import net.liftweb._
import http._
import js._
import JsCmds._
import JE._

import code.comet.TicTacToeServer

object TTTPlayerSelect {
  def render = SHtml.onSubmitUnit(() => {
    val id = S.attr("player_id")
    val symbol = ElemById("symbol_select")
    TicTacToeServer ! TicTacToeServer.ChooseSymbol(id.toString(), symbol.toString())
  })
}
