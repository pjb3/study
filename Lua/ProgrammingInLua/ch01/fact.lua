-- defines a factorial function
function fact(n)
  if n == 0 then
    return 1
  else
    return n * fact(n-1)
  end
end

print("enter a number:")
n = io.read("*number")     -- read a number
print(fact(n))