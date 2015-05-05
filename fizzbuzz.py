def FizzBuzz(i):
	Remainders = (i % 3, i % 5)
	if Remainders == (0,0):
		return "fizzbuzz"
	elif Remainders[0] == 0:
		return "fizz"
	elif Remainders[1] == 0:
		return "buzz"
	return i

print [FizzBuzz(i) for i in range(1,101)]

