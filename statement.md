
import processing.core.PConstants.MAX_INT
import processing.core._
import processing.event.KeyEvent
import scala.io.StdIn
import scala.util.Random

class RPG1 extends PApplet {

  import RPG1._

  var weapons = List.empty[Weapon]
  var enemies = List.empty[Enemy]
  var walls = List.empty[Wall]

  override def setup(): Unit = {
    characterShape = loadImage("mage.png")//loadShape("character.svg")
    targetShape = loadImage("orc.png")
  }


  override def settings(): Unit = {
    size(1600, 1000)
  }

  var lives = 100
  var xp = 0
  var level = 1
  var strength = 1
  var speed = 1
  var will = 1

  var character = Character(
    x = 100.0,
    y = 100.0,
    dx = speed,
    dy = speed,
    angle = 0.0,
    rad = 100.0
  )

  var MaxAge = 500
  var dronetime = System.currentTimeMillis()

  val PlayingState = 0
  val InventoryState = 1
  val CharacterState = 2

  var state = PlayingState

  override def draw(): Unit = {
    state match {
      case PlayingState => drawPlaying()
      case InventoryState => drawInventory()
      case CharacterState => drawCharacter()
    }
  }

  def drawPlaying(): Unit = {
    if (xp >= 50) {
      level = level + 1
      if (level >= 2) {
        strength = strength + 1
      }
      if (level >= 2) {
        strength = strength + 1
        speed = speed + 1
        will = will + 1
      }
      xp = 0
    }

    background(0, 100, 100)

    character.move(this)

    weapons = weapons.filter(weapon => weapon.age <= MaxAge)

    enemies = enemies.filter(enemy => enemy.dead == false)

    enemies foreach { enemy =>
      enemy.move()
      enemy.draw(this)
    }

    enemies foreach { enemy =>
      weapons foreach { weapon =>
        if (weapon.hit(enemy)) {
          enemy.dead = true
          xp = xp + 5
        }

      }
    }

    walls foreach { wall =>

      if (character.bounce(wall)) {
        character.x = character.x
        character.y = character.y
      }
    }
    walls foreach { wall =>
      enemies foreach { enemy =>

        if (enemy.bounce2(wall)) {
          enemy.dx = -enemy.dx
          enemy.dy = -enemy.dy
        }
      }
    }
    walls foreach { wall =>
      weapons foreach { weapon =>

        if (weapon.bounce3(wall)) {
          weapon.dx = -weapon.dx
          weapon.dy = -weapon.dy
        }
      }
    }

    if (System.currentTimeMillis() >= dronetime) {
      val enemy = Enemy(Math.random * 1000.0, Math.random * 1000.0, Math.random * 1, Math.random * 1, 100)
      enemies = enemies :+ enemy
      dronetime = dronetime + 500 + Random.nextInt(800)
    }


    weapons foreach { weapon =>
      weapon.move()
      weapon.draw(this)
    }


    character.draw(this)


    walls foreach { wall =>
      wall.draw(this)
    }

    if (lives == 0) {
      System.exit(0)
    }
  }

  var inventory = List(
    "The universe"
  )

  def drawInventory(): Unit = {
    background(50, 50, 50)
    inventory foreach { item =>
      text(item, 500, 500)
    }
  }

  var characterSheet = List(

  )

  def drawCharacter(): Unit = {
    background(255, 255, 255)
    //text(name + " The Wizard That Is Level " + level, 10, 10)
    text("xp: " + xp, 10, 50)
    text("speed: " + speed, 10, 150)
    text("will: " + will, 10, 200)


  }

  val Left = 37
  val Right = 39
  val Up = 38
  val Down = 40

  override def keyPressed(event: KeyEvent): Unit = {
    if (event.getKeyCode == Right) {
      character.x = character.x+(10*speed)
      character.dy = 0
      character.angle = -Math.PI / 2
    } else if (event.getKeyCode == Left) {
      character.x = character.x-(10*speed)
      character.dy = 0
      character.angle = -Math.PI / 2
    } else if (event.getKeyCode == Down) {
      character.y = character.y+(10*speed)
      character.dx = 0
      character.angle = -Math.PI / 2
    } else if (event.getKeyCode == Up) {
      character.dx = 0
      character.y = character.y-(10*speed)
      character.angle = -Math.PI / 2
    } else if (event.getKey == 'i') {
      state = InventoryState

    } else if (event.getKey == 'g') {
      state = PlayingState
    } else if (event.getKey == 'c') {
      state = CharacterState
    } else if (event.getKeyCode == 32) {
      if (character.angle == floater(Math.PI)) {

        weapons = weapons :+ Weapon(character.x, character.y, character.dx - 10, character.dy, 10)
      } else if (character.angle == 0) {
        weapons = weapons :+ Weapon(character.x, character.y, character.dx + 10, character.dy, 10)
      }
      else if (character.angle == floater(-Math.PI / 2)) {
        weapons = weapons :+ Weapon(character.x, character.y, character.dx, character.dy - 10, 10)
      }
      else if (character.angle == floater(-Math.PI * 3 / 2)) {
        weapons = weapons :+ Weapon(character.x, character.y, character.dx, character.dy + 10, 10)
      } else {}
    }else if(event.getKey == 'w'){
      weapons = weapons :+ Weapon(character.x, character.y, 0, (15*will), 10)
    }else if(event.getKey == 'a'){
      weapons = weapons :+ Weapon(character.x, character.y, (15*will),0 , 10)
    }else if(event.getKey == 's'){
      weapons = weapons :+ Weapon(character.x, character.y, 0, -(15*will), 10)
    }else if(event.getKey == 'd'){
      weapons = weapons :+ Weapon(character.x, character.y, -(15*will),0 , 10)
    }


    if (character.x + character.rad >= width) {
      character.x = width - character.rad - 1

    } else if (character.x - character.rad < 0) {
      character.x = character.rad
    } else if (character.y + character.rad >= height) {
      character.y = height - character.rad - 1
    } else if (character.y - character.rad < 0) {
      character.y = character.rad
    }


  }


  override def keyReleased(event: KeyEvent): Unit = {
  }


}

object RPG1 extends App {
  var class2 = 0
  //println("Welcome to WizardRPG1 By E. Hughes")
  //println("enter Name:")
  //var name = StdIn.readLine()
  //println("hello " + name)

  //println("hello " + name + " the " +  "Wizard !")

  PApplet.main("RPG1")

  var targetShape: PImage = _
  var characterShape: PImage = _

  case class Character(
                        var x: Float,
                        var y: Float,
                        var dx: Float,
                        var dy: Float,
                        var angle: Float,
                        rad: Float
                      ) {

    def move(p: PApplet): Unit = {
      if (y + dy >= p.height - rad) {
        dy = -dy
      } else if (y + dy <= rad) {
        dy = -dy
      }

      if (x + dx >= p.width - rad) {
        dx = -dx
      } else if (x + dx <= rad) {
        dx = -dx
      }

      x = x + dx
      y = y + dy


    }

    def draw(p: PApplet): Unit = {
      import p._

      pushMatrix()
      translate(x, y)
      rotate(angle + floater(Math.PI / 2))
      image(characterShape, -rad, -rad, rad * 2, rad * 2)
      popMatrix()
    }

    def bounce(wall: Wall): Boolean = {
      var wx = wall.x
      var wy = wall.y
      var wallxl = wall.xl


      var wallyl = wall.yl
      (x >= wx - rad && x <= wx + wallxl + rad && y >= wy - rad && y < wy + wallyl + rad) ||
        (y >= wy - rad && y <= wy + wallyl + rad && x >= wx - rad && x < wx + wallxl + rad)

    }
  }


  case class Wall(
                   var x: Float,
                   var y: Float,
                   var xl: Float,
                   var yl: Float

                 ) {


    def draw(p: PApplet): Unit = {
      import p._
      fill(0, 1, 1)
      rect(x, y, xl, yl)

    }


  }

  case class Escape(
                     var x: Float,
                     var y: Float,
                     var xl: Float,
                     var yl: Float

                   ) {


    def draw(p: PApplet): Unit = {
      import p._
      fill(0, 0, 255)
      rect(100, 100, 50, 50)

    }


  }

  case class Enemy(
                    var x: Float,
                    var y: Float,
                    var dx: Float,
                    var dy: Float,
                    var rad: Float
                  ) {
    var dead = false

    def move(): Unit = {
      x = x + dx
      y = y + dy
    }

    def draw(p: PApplet): Unit = {
      import p._
      fill(0, 1, 1)
      image(targetShape, x, y, 300, 300)

    }

    def bounce2(wall: Wall): Boolean = {
      var wx = wall.x
      var wy = wall.y
      var wallxl = wall.xl
      var wallyl = wall.yl
      (x >= wx - rad && x <= wx + wallxl + rad && y >= wy - rad && y < wy + wallyl + rad) ||
        (y >= wy - rad && y <= wy + wallyl + rad && x >= wx - rad && x < wx + wallxl + rad)
    }


  }

  case class Weapon(
                     var x: Float,
                     var y: Float,
                     var dx: Float,
                     var dy: Float,
                     var age: Int
                   ) {
    def move(): Unit = {
      x = x + -dx
      y = y + -dy
      age = age + 1
    }

    def draw(p: PApplet): Unit = {
      import p._
      fill(0, 0, 255)
      ellipse(x, y, 5.0, 5.0)
    }

    def kill(): Unit = {
      age = MAX_INT
    }

    def hit(enemy: Enemy): Boolean = {
      var dx = enemy.x
      var dy = enemy.y
      var bx = x
      var by = y
      var br = 2
      var dr = enemy.rad
      var a = dx - bx
      var o = dy - by
      var h = Math.sqrt(a * a + o * o)

      def bounce3(wall: Wall): Boolean = {
        var wx = wall.x
        var wy = wall.y
        var wallxl = wall.xl
        var wallyl = wall.yl
        (x >= wx - 2.5 && x <= wx + wallxl + 2.5 && y >= wy - 2.5 && y < wy + wallyl + 2.5) ||
          (y >= wy - 2.5 && y <= wy + wallyl + 2.5 && x >= wx - 2.5 && x < wx + wallxl + 2.5)
      }

      h <= dr + br


    }

    def bounce3(wall: Wall): Boolean = {
      var wx = wall.x
      var wy = wall.y
      var wallxl = wall.xl
      var wallyl = wall.yl
      (x >= wx - 2.5 && x <= wx + wallxl + 2.5 && y >= wy - 2.5 && y < wy + wallyl + 2.5) ||
        (y >= wy - 2.5 && y <= wy + wallyl + 2.5 && x >= wx - 2.5 && x < wx + wallxl + 2.5)
    }
  }

  implicit def floater(v: Double): Float = v.toFloat
}
