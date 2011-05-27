package code.snippet

import net.liftweb._
import http._
import js._
import JsCmds._
import JE._

import code.comet.ChatServer

object ChatIn {
  def render = SHtml.onSubmit(s => {
    ChatServer ! s
    SetValById("chat_in", "")
  })
}
