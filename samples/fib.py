def fib(x):
    if not (x > 1):
        return 1
    return fib(x - 1) + fib(x - 2)

x = int(input())
print(fib(x))
