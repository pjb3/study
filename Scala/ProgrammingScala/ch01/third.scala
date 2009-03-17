object Upper { 
  def main(strings: Array[String]) = { 
    strings.map(_.toUpperCase()).foreach(printf("%s ",_)) 
    println("") 
  } 
}