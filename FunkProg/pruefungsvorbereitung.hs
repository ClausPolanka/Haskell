p (a:b:l) xs ys = p l (a:xs) (b:ys);
p _ xs ys = (sum xs, product ys)
