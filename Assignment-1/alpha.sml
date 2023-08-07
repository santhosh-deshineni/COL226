fun sqrtLongDiv(n: int, d: int, res: int): int =
    if n < d * res then res
    else sqrtLongDiv(n - d * res, 2 * d, res + 1)