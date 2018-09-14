scplx (a, b) (c, d) = (a+c, b+d)
mcplx (a, b) (c, d) = (a*c - b*d, a*d + b*c)
abscx (a, b) = sqrt ( a*a + b*b )
mbrot c z = scplx (mcplx z z) c

range = foldr f1 [] [-2, -1.9.. 2]
    where f1 v1 a1 = (foldr f2 [] [-2, -1.9.. 2]):a1
            where f2 v2 a2 = (v1, v2):a2

mbrot_iterate c it = aux c (0,0) it
    where aux c z it
            | it == 0         = True
            | abscx z > 2     = False
            | otherwise       = aux c (mbrot c z) (it - 1)

mbrot_chars = map f1 range
    where f1 a = map f2 a
             where f2 b 
                     | mbrot_iterate b 20 = '*'
                     | otherwise          = ' '

mandelbrot = foldr f1 "" mbrot_chars
    where f1 val acc = (foldr (:) "" val)++('\n':acc)
        
-- putStr mandelbrot
