load('helpers.js')

Array.prototype.bubble_sort = function() {
  var sorted = false
  var passes = 0
  var n = 0
  while(!sorted) {
    sorted = true
    for(var i = 1; i < this.length; i++) {
      if(this[i] < this[i-1]) {
        this.swap(i, i-1)
        sorted = false
      }
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
  cases[c].bubble_sort()
  print('')
}