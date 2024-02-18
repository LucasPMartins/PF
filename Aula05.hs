
import Data.Char (ord, toUpper, toLower)

{-Assuntos: Tuplas, Tipos Compostos com Tuplas.-}

{-1. Defina uma função que retorne uma tupla-3 (tripla) contendo o caractere fornecido
como entrada, o mesmo caractere em letras minúsculas ou maiúsculas, e o seu número
da tabela ASCII. Exemplo:
converte b (b, B, 98)-}

converte :: Char -> (Char,Char,Int)
converte c = (toUpper c,toLower c,ord c)

--toUpper letras maiúsculas
--toLower letras minúsculas
--ord número da tabela ASCII
--Teste = converte 'a' 

{-2. Crie uma função que receba os coeficientes de uma equação do segundo grau 
𝑎𝑥^2 + 𝑏𝑥 + 𝑐 na forma (𝑎, 𝑏, 𝑐) e retorne as raízes desta equação. Trate o caso de raízes
imaginárias, indicando um erro. Exemplo:
Main> equacao (1, (-5), 6)
(2,3)
-}

equacao :: (Float,Float,Float) -> (Float,Float) 
equacao (a,b,c) |delta > 0 = (x1,x2)
                |delta == 0 = (x1,x1)
                |otherwise = (-111111111,-111111111)
                where delta = b*b - 4*a*c
                      x1 = ((-b) + sqrt delta )/ (2*a)
                      x2 = ((-b) - sqrt delta )/ (2*a)

{-3. Seja uma quantidade de registros de pessoas, em que três argumentos definem cada
registro. Os argumentos são nome, idade e sexo de uma dada pessoa.
a) Crie um novo tipo para definir uma Pessoa.
b) Cadastre algumas pessoas quaisquer, usando uma função pessoa, que receba um
número de índice (1, 2, 3, 4...) e retorne a pessoa correspondente ao índice, usando
casamento de padrões.
c) Reescreva a mesma função do item b), usando guardas.
d) Crie uma função que receba uma pessoa e retorne sua idade.
e) Crie uma função recursiva que some as idades de todas as pessoas cadastradas.
Dica: use a função do item d).
f) Crie uma função que retorne a média da idade de todas as pessoas dentre a pessoa
do registro 1 até o registro n, que é o número passado como argumento nesta
função. Dica: use a função do item e). -}

data Pessoa = Pessoa { nome :: String, idade :: Int, sexo :: Char } deriving Show

{- b)
pessoa :: Int -> Pessoa
pessoa 1 = Pessoa "Ana" 25 'F'
pessoa 2 = Pessoa "João" 30 'M'
pessoa 3 = Pessoa "Maria" 20 'F'
-- Adicione mais pessoas conforme necessário
-}

pessoa :: Int -> Pessoa
pessoa n
    | n == 1 = Pessoa "Ana" 25 'F'
    | n == 2 = Pessoa "João" 30 'M'
    | n == 3 = Pessoa "Maria" 20 'F'
    | otherwise = error "Índice de pessoa inválido"

idadeDaPessoa :: Pessoa -> Int
idadeDaPessoa (Pessoa _ idade _) = idade

somaIdades :: [Pessoa] -> Int
somaIdades [] = 0
somaIdades (p:ps) = idadeDaPessoa p + somaIdades ps

mediaIdades :: Int -> Int -> Float
mediaIdades 0 _ = 0
mediaIdades n total = fromIntegral total / fromIntegral n

main :: IO ()
main = do
    let pessoa1 = pessoa 1
    let pessoa2 = pessoa 2
    let pessoa3 = pessoa 3
    let listaPessoas = [pessoa1, pessoa2, pessoa3]
    putStrLn $ "Idade da pessoa 1: " ++ show (idadeDaPessoa pessoa1)
    putStrLn $ "Soma das idades: " ++ show (somaIdades listaPessoas)
    putStrLn $ "Média das idades: " ++ show (mediaIdades 3 (somaIdades listaPessoas))


