module Main (main) where
 
import System.Random
import System.IO (stdout,hSetBuffering,BufferMode(NoBuffering))

mainLoop :: IO ()
mainLoop = do hSetBuffering stdout NoBuffering
              numAleatorio <- randomRIO (1, 100) :: IO Int
              putStrLn "Estou pensando em um número entre 1 e 100. Tente adivinhar.\n\n"
              tentativa <- try 1 numAleatorio
              recorde tentativa
              putStr "Digite 'S' para continuar ou qualquer outra coisa para parar:"
              input <- getLine
              putStrLn ""
              if (input == "S" || input == "s") then mainLoop
                else  putStrLn "\nObrigado por jogar o Jogo de Adivinhação!\n"

main :: IO () 
main = do 
    putStrLn "\nBem-vindo ao Jogo da Adivinhação!\n"
    mainLoop -- Inicia o loop

try :: Int -> Int -> IO Int
try a b = do
    putStr ("Tentativa " ++ show a ++ ":\n")
    putStr "Digite seu palpite:"
    num <- getLine
    if read num == b then do
        putStrLn ("Parabéns! Você acertou em " ++ show a ++ " tentativas.\n\n")
        return a
    else if read num < b then do
        putStrLn ("Seu palpite de " ++ show num ++ " está abaixo do número correto.\n\n")
        try (a + 1) b
    else do
        putStrLn ("Seu palpite de " ++ show num ++ " está acima do número correto.\n\n")
        try (a + 1) b

recorde :: Int -> IO ()
recorde a = do
    conteudo <- readFile "highscore.txt"
    let record = read conteudo :: Int 
    if a < record then do
        putStrLn "Você bateu o recorde!\n\n"
        writeFile "highscore.txt" (show a)
    else
        return ()
 
