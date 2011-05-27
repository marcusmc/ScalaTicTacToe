package code.snippet

import net.liftweb._
import util._
import Helpers._
import http._
import scala.xml.NodeSeq

class Recurse(which: Which) {
	def render = which match {
	  case First() => "#first ^^"  #> "*"
      case Second() => "#second ^^"  #> "*"
      case Both() => ClearClearable
	}
}

sealed trait Which
final case class First() extends Which
final case class Second() extends Which
final case class Both() extends Which


object FirstTemplate {
  def render(in: NodeSeq)= {
    S.notice("First Template Snippet executed")
    in
  }
}

object SecondTemplate {
  def render(in: NodeSeq)= {
    S.notice("Second Template Snippet executed")
    in
  }
}
