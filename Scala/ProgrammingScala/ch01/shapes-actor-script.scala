import shapes._

ShapeDrawingActor.start()
ShapeDrawingActor ! Circle(Point(0.0,0.0), 1.0)
ShapeDrawingActor ! Rectangle(Point(0.0,0.0), 2, 5)
ShapeDrawingActor ! 3.14159
ShapeDrawingActor ! "exit"