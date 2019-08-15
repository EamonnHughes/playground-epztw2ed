


import sun.management.snmp.jvmmib.JvmRTBootClassPathEntryMBean

import scala.io.StdIn


object Adventure extends App {

  // Some Types

  type Actions = PartialFunction[List[String], Unit]

  trait Place {
    var things: Set[String] = Set.empty

    def describe(): Unit

    def actions: Actions
  }

  // Some Variables

  var place: Place = Shire
  var inventory: Set[String] = Set.empty
  var going: Boolean = true

  // Some Things

  def Ring = "Ring"

  def Banana = "Banana"

  def Boat = "Boat"

  // Some Places

  case object Shire extends Place {
    things = things + Ring


    def describe(): Unit = {
      println("You are in the shire.")
    }

    def actions: Actions = {
      case "go" :: "out" :: Nil =>
        place = Bree
    }
  }

  case object Bree extends Place {
    def describe(): Unit = {
      println("You come to bree, a town of Humans and Hobbits. there a barman, named Butterbur gives you a letter. You also meet a human named Strider who will help you later")
    }

    def actions: Actions = {
      case "Read" :: "letter" :: Nil =>
        println("meet me at the house of elrond(east)." +
          " yours, gandalf")




      case "go" :: "west" :: Nil =>
        place = Shire

      case "go" :: "east" :: Nil =>
        place = weathertop

    }
  }

  case object GreatNorthRoad extends Place {
    def describe(): Unit = {
      println("You are on a great dark road going north-south.")
    }

    def actions: Actions = {
      case "go" :: "north" :: Nil =>

      case "go" :: "south" :: Nil =>
        place = Bree
    }
  }

  case object weathertop extends Place {
    things = things + Banana

    var happy: Boolean = false

    def describe(): Unit = {
      println("You are on a big lane that runs east.")
      println("There is a great big dog in the lane.")
      if (happy) {
        println("The dog is pleased to see you.")
      }
    }

    def actions: Actions = {
      case "go" :: "west" :: Nil =>
        place = Bree

      case "go" :: "east" :: Nil =>
        if (happy) {
          place = NearEast
        } else {
          println("The dog growls angrily and you run away home.")
          place = Shire
        }

      case "pet" :: "dog" :: Nil =>
        println("The dog wags his tail.")
        happy = true

      case "kick" :: "dog" :: Nil =>
        println("The dog eats you.")
        going = false
    }
  }

  case object NearEast extends Place {
    def describe(): Unit = {
      println("You are on a little lane that runs east.")
      println("There is a great big Spider in the lane.")

    }

    def actions: Actions = {
      case "go" :: "west" :: Nil =>
        place = weathertop

      case "go" :: "east" :: Nil =>
        if (inventory.contains(Ring)) {
          place = EndOfTheRoad
        } else {
          println("The cat eats you.")
          going = false
        }

      case "pet" :: "cat" :: Nil =>
        println("The cat eats you.")
        going = false
    }
  }

  case object EndOfTheRoad extends Place {
    def describe(): Unit = {
      things = things + Boat
      println("You have reached a river.")

    }

    def actions: Actions = {
      case "C" :: "C" :: Nil =>
        place = Island

      case "go" :: "home" :: Nil =>
        place = Shire
    }
  }

  case object Island extends Place {
    def describe(): Unit = {
      println("The boat takes you to an island. You won.")
      going = false
    }
    def actions: Actions = {

      case "go" :: "home" :: Nil =>
        place = Shire
    }
  }


  // Some Functions

  def lookAround(): Unit = {
    place.describe()
    if (place.things.nonEmpty) {
      val things = place.things.mkString(", ")
      println(s"Things you see here: $things")
    }
  }

  def printInventory(): Unit = {
    if (inventory.nonEmpty) {
      val things = inventory.mkString(", ")
      println(s"Things you have: $things")
    } else {
      println("You have nothing.")
    }
  }

  def take(thing: String): Unit = {
    if (place.things.contains(thing)) {
      place.things = place.things - thing
      inventory = inventory + thing
      printInventory()
    } else {
      println(s"There is no $thing here.")
    }
  }


  def drop(thing: String): Unit = {
    if (inventory.contains(thing)) {
      inventory = inventory - thing
      place.things = place.things + thing
      printInventory()
    } else {
      println(s"You do not have a $thing.")
    }
  }

  def eat(thing: String): Unit = {
    if (inventory.contains(thing)) {
      inventory = inventory - thing
      if (thing == Banana) {
        println("Not bad.")
      } else {
        println("You do not feel very well.")
      }
    } else {
      println(s"You do not have a $thing.")
    }
  }

  def defaultActions: Actions = {

    case "inventory" :: Nil =>
      printInventory()

    case "look" :: Nil =>
      lookAround()

    case "take" :: thing :: Nil =>
      take(thing)

    case "drop" :: thing :: Nil =>
      drop(thing)

    case "eat" :: thing :: Nil =>
      eat(thing)

    case "go" :: where :: Nil =>
      println(s"You cannot go $where.")

    case "what" :: Nil =>
      println("You can: inventory (see inventory), look (see place), take _____(pick something up), drop ______(put something down), eat ________(eat something), go ______(go somewhere), quit(quit), other things")

    case "quit" :: Nil =>
      println("Bye.")
      going = false

    case "win" :: Nil =>
      println("You won!??!?!??!?!??!?!??!")
      going = false

    case _ =>
      println("I do not know how to do that.")
  }

  def perform(action: List[String]): Unit = {
    val actions = place.actions.orElse(defaultActions)
    actions(action)
  }

  // The main program

  while (going) {
    lookAround()
    val action = StdIn.readLine("What now? ")
    val words = action.split(" ").toList
    perform(words)
  }

}



