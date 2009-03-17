package shapes {
  case class Point(x: Double, y: Double)
  
  abstract case class Shape() {
    def draw(): Unit
  }
  case class Circle(center: Point, radius: Double) extends Shape {
    def draw() = println("Circle.draw: " + this)
  }
  case class Rectangle(lowerLeft: Point, height: Double, width: Double) extends Shape {
    def draw() = println("Rectangle.draw: " + this)
  }
}