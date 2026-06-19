def fibonacci(n):
    a, b = 0, 1
    for _ in range(n):
        print(a, end=" ")
        a, b = b, a + b

# Mostrar los primeros 10 números de Fibonacci
fibonacci(10)