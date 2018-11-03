object Greeter {
  def printGreeting(name : String): Unit = printf("Welcome %s!", name)
}

class HelloWorld(name : String) {
  import Greeter.printGreeting
  def helloWorld(): Unit = {println("Hello world!"); printGreeting(name)}
}

new HelloWorld("Scala").helloWorld()

def map[T](fn : T => T, list : List[T]) : List[T] = {
  list match {
    case x::more => fn(x) :: map(fn, more)
    case _ => List()
  }
}

map((x : Int) => x + 1, List(1, 2, 3, 4, 5))

def filter[T](fn : T => Boolean, list : List[T]) : List[T] = {
  list match {
    case x::more => if (fn(x)) x :: filter(fn, more) else filter(fn, more)
    case _ => List()
  }
}

filter((x : Int) => x % 2 == 0, List(1, 2, 3, 4, 5, 6))