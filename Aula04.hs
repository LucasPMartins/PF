{-Assuntos: Recursão, Casamento de Padrões, Avaliação Preguiçosa-}

{-1. O fatorial duplo de um número natural n é o produto de todos os números de 1 (ou 2)
até n, contados de 2 em 2. Por exemplo, o fatorial duplo de 8 é 8 × 6 × 4 × 2 = 384, e
o fatorial duplo de 7 é 7 × 5 × 3 × 1 = 105. Defina uma função para calcular o fatorial
duplo usando recursividade.-}

facduplo :: Int -> Int
facduplo x
  | x <= 0 = 1
  | otherwise = x * facduplo (x - 2)

{-2. Defina duas funções recursivas que calculam o quociente e o resto da divisão inteira
de dois números naturais usando subtrações sucessivas.-}

resto :: Int -> Int -> Int
resto x y
  | x == y = 0
  | x < y = x
  | x > y = resto (x - y) y

quociente :: Int -> Int -> Int
quociente x y
  | x == y = 1
  | x < y = 0
  | x > y = 1 + quociente (x - y) y

{-3. Defina uma função recursiva para calcular a potência de um número, considerando que
o expoente é um número natural. Utilize o método das multiplicações sucessivas.-}

potencia :: Int -> Int -> Int
potencia x y
  | y == 0 = 1
  | otherwise = x * potencia x (y - 1)

{-4. Considere o operador nand, que devolve True se ao menos um dos argumentos é False.
(a) Defina a função nand utilizando if then else;
(b) Defina a função nand utilizando guardas;
(c) Defina a função nand utilizando correspondência de padrões; -}

nand1 :: Bool -> Bool -> Bool
nand1 x y = if x == False || y == False then True else False

nand2 :: Bool -> Bool -> Bool
nand2 x y
  | x == False = True
  | y == False = True
  | otherwise = False

nand3 :: Bool -> Bool -> Bool
nand3 True True = False
nand3 _ _ = True