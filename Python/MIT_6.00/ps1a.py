# Problem Set 1a
# Name: Paul Barry
# Time: 0:15
prime_count = 1
n = 1
while(prime_count < 1000):
  n = n + 2
  i = 3
  while(i <= n):
    if(i == n):
      prime_count = prime_count + 1
      print(str(n) +" is the " + str(prime_count) +"th prime")
    elif(n%i==0):
      # n is divisible by i, so it is not prime
      # so we can stop now
      break
    i = i + 1
      