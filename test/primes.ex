integer pc, k, p, lim, n

lim = 5000000
n = 1
pc = 0


while n < lim do
    k = 3
    p = 1
    n = n + 2
    while k * k < n and p do
        p = floor(n / k) * k != n
        k = k + 2
    end while
    if p then
        pc = pc + 1
    end if
end while

? pc
