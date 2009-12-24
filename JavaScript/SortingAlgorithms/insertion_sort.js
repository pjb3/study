load('helpers.js')

Array.prototype.insertion_sort = function() {
  var passes = 0
  var n = 0
  for(var i = 1; i < this.length; i++) {
    var j = i
    while((j>0) && (this[j] < this[j-1])) {
      this.swap(j, j-1)
      j--
      n++
    }
    passes++
    print('  After pass '+passes+', n = '+n+', items = '+this)
  }  
}

var cases = {
  sorted: range(1, 10),
  random: range(1, 10).randomize(),
  reversed: range(1, 10).reverse()
}

for(var c in cases) {
  print(c+' case')
  cases[c].insertion_sort()
  print('')
}