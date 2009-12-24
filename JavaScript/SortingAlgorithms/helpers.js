function range(from, to) {
  var result = []
  for(var i = from; i <= to; i++) {
    result.push(i)
  }
  return result
}

Array.prototype.randomize = function() {
  this.sort(function(){ return (Math.round(Math.random()) - 0.5) })
  return this
}

Array.prototype.toStringWithoutBrackets = Array.prototype.toString
Array.prototype.toString = function() {
  return '['+this.toStringWithoutBrackets()+']'
}

Array.prototype.swap = function(a, b) {
  var tmp = this[a]
  this[a] = this[b]
  this[b] = tmp
}

Array.prototype.first = function() {
  if(this.length > 0) {
    return this[0]  
  } else {
    return null
  }
}

Array.prototype.rest = function() {
  if(this.length > 0) {
    return this.slice(1)
  } else {
    return []
  }
}

Array.prototype.last = function() {
  if(this.length > 0) {
    return this[this.length - 1]  
  } else {
    return null
  }
}

Array.prototype.append = function(array) {
  this.push.apply(this, array)
  return this
}
