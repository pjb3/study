load('helpers.js')

Array.prototype.divide = function() {
  var i = Math.floor(this.length/2)
  if(this.length%2 == 1) {
    i++
  }
  return [this.slice(0,i), this.slice(i)]
}

Array.prototype.merge = function(array) {
  var result = []
  var left = this
  var right = array
  while(left.length > 0 && right.length > 0) {
    if(left.first() <= right.first()) {
      result.push(left.first())
      left = left.rest()
    } else {
      result.push(right.first())
      right = right.rest()
    }
  }
  if(left.length > 0) {
    result.append(left)
  } else {
    result.append(right)
  }
  return result
}

var merge_sort_depth = 0
Array.prototype.merge_sort = function() {
  var indent = ''
  for(var i = 0; i < merge_sort_depth; i++) {
    indent += '  '
  }
  print(indent+"merge sort "+this)
  
  if(this.length <= 1) {
    return this
  }
  
  var result = []
  var divided = this.divide()
  merge_sort_depth++
  var left = divided[0].merge_sort()
  var right = divided[1].merge_sort()
  merge_sort_depth--
  
  if(left.last() > right.last()) {
    result = left.merge(right)
  } else {
    result = left.append(right)
  }
  
  return result
}

var cases = {
  sorted: range(1, 10),
  random: range(1, 10).randomize(),
  reversed: range(1, 10).reverse()
}

for(var c in cases) {
  print(c+' case')
  var result = cases[c].merge_sort()
  print(cases[c]+' sorted into '+result)
  print('')
}

