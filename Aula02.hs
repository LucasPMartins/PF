{-Assuntos: Expressões condicionais, equações com guardas, blocos de comandos e
definições locais.-}

{-1. O operador “ou exclusivo” (xor, denotado por ⊗) considera dois valores lógicos e
devolve True quando estes valores são diferentes e False quando são iguais:
𝑎 ⊗ 𝑏 = (𝑎 ∨ 𝑏) ∧ ¬(𝑎 ∧ 𝑏)
(a) Defina a função xor utilizando álgebra Booleana;
(b) Defina a função xor utilizando o operador relacional /=;
(c) Defina a função xor utilizando if then else;
(d) Defina a função xor utilizando guardas.-}

xor1 :: Bool -> Bool -> Bool
xor1 a b = (a || b) && not (a && b)

xor2 :: Bool -> Bool -> Bool
xor2 a b = a /= b

xor3 :: Bool -> Bool -> Bool
xor3 a b = if a == b then False else True

xor4 :: Bool -> Bool -> Bool
xor4 a b
  | a == b = False
  | otherwise = True

{-2. Faça uma função par que recebe um número inteiro e devolve verdadeiro se o número
for par e falso, caso contrário. Não se esqueça das definições de tipos.-}

par :: Int -> Bool
par a = if mod a 2 == 0 then True else False

{-3. Utilizando a função do item anterior, implemente a função impar que recebe um
número inteiro e devolve verdadeiro se o número for ímpar e falso, caso contrário.-}

impar :: Int -> Bool
impar a = if par a == True then False else True

{-4. Crie as funções maior e menor, que se comportam da seguinte maneira:
> maior 2 4
4
> menor 2 4
2
Não se esqueça de declarar corretamente os tipos (assinatura) dessas funções. Em
seguida, explique o funcionamento da seguinte função:
1
diferenca x y = l - s
where
l = maior x y s =
menor x y-}

maior :: Float -> Float -> Float
maior a b
  | a > b = a
  | otherwise = b

menor :: Float -> Float -> Float
menor a b
  | a < b = a
  | otherwise = b

diferenca :: Float -> Float -> Float
diferenca x y = l - s
  where
    l = maior x y
    s = menor x y

{-5. Utilizando o comando where, implemente uma função que receba o diâmetro de um
círculo e calcule sua área.-}

areaC :: Float -> Float
areaC d = pi * r * r
  where
    r = d / 2

{-6. Defina uma função chamada numRaizes, que recebe os três coeficientes de uma
equação do segundo grau:
𝑎𝑥^2 + 𝑏𝑥 + 𝑐 = 0
e calcula a quantidade de raízes reais distintas da equação. Lembrando que, para isso,
é necessário calcular o discriminante ∆ da equação:
∆ = 𝑏^2 − 4ac.
Use uma definição local para calculá-lo. Se ∆ for positivo, a equação tem duas reais
reais e distintas, se for nulo, a equação tem uma raiz real, e se for negativo, a equação
não tem raízes reais.-}

numRaizes :: Float -> Float -> Float -> Int
numRaizes a b c
  | delta == 0 = 1
  | delta > 0 = 2
  | otherwise = 0
  where
    delta = b * b - 4 * a * c

{-7. Considere o menu de opções abaixo:
1 - Soma de 2 números.
2 - Diferença entre 2 números (maior pelo menor).
3 - Produto entre 2 números.
4 - Divisão entre 2 números (maior dividido pelo menor. Se o denominador for nulo, retornar como resultado o valor -1.
Leia dois numeros inteiros, seguidos pela opção escolhida. Retorne como resultado o
valor -1, caso o usuário digite uma operação que não ao pertence ao menu. -}

menu :: Float -> Float -> Int -> Float
menu a b op = case op of
  1 -> a + b
  2 -> diferenca a b
  3 -> a * b
  4 -> if b == 0 then -1 else a / b
  _ -> -1
