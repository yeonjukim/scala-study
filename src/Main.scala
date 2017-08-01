import scala.util.Random
import scala.util.matching.Regex

/**
  * Created by kakao on 2017. 7. 26..
  */
object Main {
  def main(args: Array[String]): Unit = {
    val plus = (x : Int, y : Int) => x + y

    val minus = (x : Int, y : Int) => x - y

    val multiply = (f1 : (Int, Int) => Int, x : Int, y : Int, z : Int) => f1(x, y) * z

    println(multiply(plus, 2, 3, 2))
    println(multiply(minus, 10, 5, 1))

    abstract class Expr
    case class Var(name: String) extends Expr
    case class Number(num: Double) extends Expr
    case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

    def simplifyTop (expr: Expr) = expr match {
      case BinOp("+", e, Number(0)) => println("a deep match")
      case _ =>
    }
    val binOp = BinOp("-", Number(1), Number(0))
    simplifyTop(binOp)
  }
  abstract class Notification
  case class Email(sender: String, title: String, body: String) extends Notification
  case class SMS(caller: String, message: String) extends Notification
  case class VoiceRecording(contactName: String, link: String) extends Notification

  def showNotification(notification: Notification): String = {
    notification match {
      case Email(email, title, _) =>
        s"You got an email from $email with title: $title"
      case SMS(number, message) =>
        s"You got an SMS from $number! Message: $message"
      case VoiceRecording(name, link) =>
        s"you received a Voice Recording from $name! Click the link to hear it: $link"
    }
  }
  def showImportantNotification(notification: Notification, importantPeopleInfo: Seq[String]): String = {
    notification match {
      case Email(email, _, _) if importantPeopleInfo.contains(email) =>
        "You got an email from special someone!"
      case SMS(number, _) if importantPeopleInfo.contains(number) =>
        "You got an SMS from special someone!"
      case other =>
        showNotification(other) // nothing special, delegate to our original showNotification function
    }
  }
  val importantPeopleInfo = Seq("867-5309", "jenny@gmail.com")
  val someSms = SMS("867-5309", "Are you there?")
  val someVoiceRecording = VoiceRecording("Tom", "voicerecording.org/id/123")
  val importantEmail = Email("jenny@gmail.com", "Drinks tonight?", "I'm free after 5!")
  val importantSms = SMS("867-5309", "I'm here! Where are you?")
  println(showImportantNotification(someSms, importantPeopleInfo))
  println(showImportantNotification(someVoiceRecording, importantPeopleInfo))
  println(showImportantNotification(importantEmail, importantPeopleInfo))
  println(showImportantNotification(importantSms, importantPeopleInfo))


  val numberPattern: Regex = "awesomepasswordw".r
  numberPattern.findFirstMatchIn("awesomepassword") match {
    case Some(_) => println("Password OK")
    case None => println("Password must contain a number")
  }

  object CustomerID {
    def apply(name: String) = s"$name"
    def unapply(customerID: String): Option[String] = {
      val name = customerID.split("--").head
      if (name.nonEmpty) Some(name) else None
    }
  }
  val customer1ID = CustomerID("Sukyoung")  // Sukyoung--23098234908
  customer1ID match {
    case CustomerID(name) => println(name)  // prints Sukyoung
    case _ => println("Could not extract a CustomerID")
  }
  val customer2ID = CustomerID("nico")
  val CustomerID(name) = customer2ID
  println(name)


  class Stack[A] {
    private var elements: List[A] = Nil
    def push(x: A) { elements = x :: elements }
    def peek: A = elements.head
    def pop(): A = {
      val currentTop = peek
      elements = elements.tail
      currentTop
    }
  }
  val stack = new Stack[String]
  stack.push("first")
  stack.push("second")
  println(stack.pop)  // prints 2
  println(stack.pop)  // prints 1

//  abstract class Animal {
//    def name: String
//  }
//  case class Cat(name: String) extends Animal
//  case class Dog(name: String) extends Animal
//  def printAnimalNames(animals: List[Animal]): Unit = {
//    animals.foreach { animal =>
//      println(animal.name)
//    }
//  }
//
//  val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
//  val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))
//
//  printAnimalNames(cats)
//  // Whiskers
//  // Tom
//  printAnimalNames(dogs)
//  // Fido
//  // Rex
//  abstract class Printer[-A] {
//    def print(value: A): Unit
//  }
//  class AnimalPrinter extends Printer[Animal] {
//    def print(animal: Animal): Unit =
//      println("The animal's name is: " + animal.name)
//  }
//  class CatPrinter extends Printer[Cat] {
//    def print(cat: Cat): Unit =
//      println("The cat's name is: " + cat.name)
//  }
//  val myCat: Cat = Cat("Boots")
//  def printMyCat(printer: Printer[Cat]): Unit = {
//    printer.print(myCat)
//  }
//
//  val catPrinter: Printer[Cat] = new CatPrinter
//  val animalPrinter: Printer[Animal] = new AnimalPrinter
//  printMyCat(catPrinter)
//  printMyCat(animalPrinter)

  object EMail {
    // injection (선택적)
    //def apply( user: String, domain: String ) = user + "@" + domain
    // extraction (필수)
    def unapply( str: String ) : Option[ (String, String) ] = {
      val parts = str split "@"
      if ( parts.length == 2 ) Some( parts(0), parts(1) ) else None
    }
  }
  val selectorString = "judy.kim@kakaocorp.com"
  selectorString match {
    case EMail(user, domain) => println(user + " AT " + domain)
    case _ => println("not email address")
  }

  class Rectangle{
    var width: Double = 0
    var height: Double = 0
    def setWidth(width: Double)= this.width = width
    def setHeight(height: Double)= this.height = height
    def getWidth(): Double = width
    def getHeight(): Double = height
    def getArea(): Double = width * height
  }

  class Square extends Rectangle{

    @Override
    override def setWidth(width: Double)={
      this.width = width
      this.height = width
    }

    @Override
    override def setHeight(height: Double)={
      this.height = height
      this.width = height
    }
  }
  def increaseHeight(rectangle: Rectangle): Unit = {
    if(rectangle.getHeight() <= rectangle.getWidth()){
      rectangle.setHeight(rectangle.getWidth() + 10 )
    }
  }
  val rec = new Rectangle
  rec.setWidth(4)
  rec.setHeight(3)
  increaseHeight(rec)
  println(rec.getWidth())
  println(rec.getHeight())

  case class ListNode[+T](h: T, t: ListNode[T]) {
    def head: T = h
    def tail: ListNode[T] = t
    def prepend[U >: T](elem: U): ListNode[U] =
      ListNode(elem, this)
  }
  val empty: ListNode[Null] = ListNode(null, null)
  val strList: ListNode[String] = empty.prepend("hello")
    .prepend("world")
  val anyList: ListNode[Any] = strList.prepend(12345)
  println(anyList)

  trait Abstract {
    type T
    def transform(x: T) : T
    val initial: T
    var current: T
  }
  class Concrete extends Abstract {
    type T = String
    def transform(x: String) = x + x
    val initial = "hi"
    var current = initial
  }
  class Food
  abstract class Animal {
    type SuitableFood <: Food
    def eat(food: SuitableFood)
  }
  class Grass extends Food
  class Cow extends Animal {
    type SuitableFood = Grass
    override def eat(food: SuitableFood){ }
  }
  class Fish extends Food
  val bessy: Animal = new Cow
  bessy eat (new Fish) //error
}