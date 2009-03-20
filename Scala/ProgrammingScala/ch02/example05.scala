val stateCapitals = Map( 
    "Alabama" -> "Montgomery", 
    "Alaska"  -> "Juneau", 
    "Wyoming" -> "Cheyenne") 
println("Alabama's capital is "+stateCapitals.get("Alabama")) 
println("Wyoming's capital is "+stateCapitals.get("Wyoming")) 
println("Unknown's capital is "+stateCapitals.get("Unknown")) 
println("Get the values themselves:") 
println("Alabama's capital is "+stateCapitals.get("Alabama").get) 
println("Wyoming's capital is "+stateCapitals.get("Wyoming").getOrElse("Oops!")) 
println("Unknown's capital is "+stateCapitals.get("Unknown").getOrElse("Oops2!"))
