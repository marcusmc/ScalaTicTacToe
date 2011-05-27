package code.comet

import net.liftweb._
import http._
import util._
import Helpers._

/**
 * The screen real esatte on the browser will be represented 
 * by this component.  When the component changes on the server
 * the changes are automatically reflected in the browser.
 */

class Chat extends CometActor with CometListener {
	private var msgs: Vector[String] = Vector() //private state
 
	/**
	* When the component is instantiated, register as
	* a listener with the ChatServer	
	*/
   
   def registerWith = ChatServer
 
   /**
    * The Comet Actor is an Actor, so it processes mesagges.
    * In this case, we're listening for Vector[String],
    * and when we get one, update our private state
    * and reRender() the component.  reRender() will 
    * cause changes to be sent to the browser.
    */
 
    override def lowPriority = {
      case v: Vector[String] => msgs = v; reRender()
    }
    
    /**
     * Put the messages in the li elements and 
     * clear any elements that have the clearable class.
     */
    def render = "li *" #> msgs & ClearClearable
  }
