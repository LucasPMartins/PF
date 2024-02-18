import System.IO
import Text.Read (get, readMaybe)

{-Assuntos: Funções padrão de entrada e saída; a função main; operações em arquivos-}

{-Usando esta função, escreva um programa em Haskell que solicita ao usuário digitar
uma frase, lê a frase (uma linha) da entrada padrão e testa se a string lida é um
palíndrome, exibindo uma mensagem apropriada.
Exemplo de execução:
>Digite uma frase:
abcddcba
>É um palíndrome-}

palindromo :: IO ()
palindromo = do
  putStrLn "Digite uma frase:"
  str <- getLine
  if str == reverse str
    then putStrLn "É um palíndromo"
    else putStrLn "Não é um palíndromo"

{-2. Escreva um programa que solicita ao usuário três números em ponto flutuante, lê os
números, e calcula e exibe o produto dos números.
Dica: A operação de multiplicação é definida para todos os tipos numéricos. Caso não
seja definido um tipo, o tipo Integer é escolhido. Portanto, será necessária uma
anotação de tipo em qualquer subexpressão do programa, para que este funcione com
números em ponto flutuante.-}

produto :: IO ()
produto = do
  putStrLn "Digite três números em ponto flutuante!"
  putStr "Número 1:"
  num1 <- getLine >>= readFloat
  putStr "Número 2:"
  num2 <- getLine >>= readFloat
  putStr "Número 3:"
  num3 <- getLine >>= readFloat
  let resultado = num1 * num2 * num3
  putStrLn "Resultado do produto dos três números:"
  print resultado

readFloat :: String -> IO Float
readFloat str = case readMaybe str of
  Just x -> return x
  Nothing -> do
    putStrLn "Erro: Por favor, digite um número em ponto flutuante válido!"
    getLine >>= readFloat

{-3. Escreva um programa em Haskell que solicita ao usuário uma temperatura na escala
Fahrenheit, lê esta temperatura, converte-a para a escala Celsius e exibe o resultado.
Para fazer a conversão, defina uma função celsius :: Double -> Double que
recebe a temperatura na escala Fahrenheit e resulta na temperatura correspondente
na escala Celsius. Use a seguinte equação para a conversão:
C = 5/9 × (F − 32)
onde F é a temperatura em Fahrenheit e C é a temperatura em Celsius.
Use a função celsius na definição de main.
A digitação da temperatura em Fahrenheit deve ser feita na mesma linha onde é
exibida a mensagem que a solicita.
Exemplo de execução:
>Digite uma temperatura em Fahrenheit
100
>Temperatura em Fahrenheit: 100
Temperatura em Celsius: 37.77777777777778 -}

celsius :: Double -> Double
celsius f = 5 / 9 * (f - 32)

main :: IO ()
main = do
  putStrLn "Digite uma temperatura em Fahrenheit"
  f <- getLine
  putStr "Temperatura em Fahrenheit: "
  putStrLn f
  putStr "Temperatura em Celsius: "
  let c = celsius (read f)
  putStrLn (show c)

{-4. Faça uma função que receba três notas de um aluno, e calcule e mostre a média
aritmética das notas e a situação do aluno, dadas pela tabela a seguir.
Média das notas Situação
Menor que 3 Reprovado
Entre 3 (inclusive) e 7 Exame especial
Acima de 7 (inclusive) Aprovado-}

aluno :: IO ()
aluno = do
  putStrLn "Digite três notas do Aluno!"
  putStr "Nota 1: "
  n1 <- getLine
  putStr "Nota 2: "
  n2 <- getLine
  putStr "Nota 3: "
  n3 <- getLine
  let media = (read n1 + read n2 + read n3) / 3
  if media < 3
    then putStrLn "Reprovado!"
    else
      if media <= 3 && media < 7
        then putStrLn "Exame especial!"
        else putStrLn "Aprovado!"

{-5. Crie um programa que leia a idade de uma pessoa e informe a sua classe eleitoral: não
eleitor abaixo de 16 anos; eleitor obrigatório entre 18 (inclusive) e 65 anos; eleitor
facultativo de 16 até 18 anos e acima de 65 anos (inclusive).
Exemplo de execução:
>Classe eleitoral
>-----------------------------
>Digite a idade da pessoa: 11
não eleitor-}

classe :: IO ()
classe = do
  putStrLn "Classe Eleitoral"
  putStrLn "-----------------------------"
  putStrLn "Digite sua idade:"
  i <- getLine
  let r = read i
  if r < 16
    then putStrLn "não eleitor"
    else
      if r >= 18 && r < 65
        then putStrLn "eleitor obrigatório"
        else
          if r > 16 && r < 18 || r > 65
            then putStrLn "eleitor facultativo"
            else putStrLn ""

{-6. Faça um programa que apresente um menu, o qual permita ao usuário escolher a opção
desejada, dentre:
1. Salvar uma frase em um arquivo texto
2. Imprimir o conteúdo do arquivo texto
3. Sair
A seguir, o usuário deve digitar uma opção desejada, a qual executará a ação solicitada.
Teste a opção inválida. O menu deve ser repetido infinitamente.-}

-- Função para salvar uma frase em um arquivo texto
salvarFrase :: IO ()
salvarFrase = do
  putStrLn "Digite a frase que deseja salvar:"
  frase <- getLine
  writeFile "Aula03.txt" frase
  putStrLn "Frase salva com sucesso!"

-- Função para imprimir o conteúdo do arquivo texto
imprimirConteudo :: IO ()
imprimirConteudo = do
  conteudo <- readFile "Aula03.txt"
  putStrLn "Conteúdo do arquivo texto:"
  putStrLn conteudo

-- Função principal que exibe o menu e lida com as opções do usuário
main2 :: IO ()
main2 = do
  putStrLn "Menu:"
  putStrLn "1. Salvar uma frase em um arquivo texto"
  putStrLn "2. Imprimir o conteúdo do arquivo texto"
  putStrLn "3. Sair"
  putStrLn "Escolha uma opção:"

  opcao <- getLine
  case opcao of
    "1" -> do
      salvarFrase
      main2
    "2" -> do
      imprimirConteudo
      main2
    "3" -> putStrLn "Saindo do programa."
    _ -> do
      putStrLn "Opção inválida. Escolha novamente."
      main2