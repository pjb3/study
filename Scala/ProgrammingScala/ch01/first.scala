class Upper { 
 def upper(strings: String*): Collection[String] = { 
    strings.map((s:String) => s.toUpperCase()) 
  } 
} 
val up = new Upper 
Console.println(up.upper("A", "First", "Scala", "Program"))