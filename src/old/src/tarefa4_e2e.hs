{-
    Para correr o teste end2end:

     - Abrir o ghci
     - :l tarefa4_e2e.hs"
     - tarefa4_e2e

    Nota: Todos os ficheiros .out de teste para a tarefa4 têm de acabar com uma nova linha (ou seja, tem-se de deixar uma linha vazia no final)
          Isso gastou-me quase uma tarde a comparar strings, porque eu estava a imprimi-las char a char, só que fiz copy paste e esqueci-me de mudar um valor.
          Por outas palavras, estava a imprimir duas vezes a mesma string e a perguntar-me porque é que eram diferentes >8-/
-}

module End2End where

import Data.List
import System.Directory
import Tarefa4

tarefa4_e2e :: IO ()
tarefa4_e2e = do
    files <- getDirectoryContents "../tests/tarefa4/"
    let inputs = map ("../tests/tarefa4/" ++) $ filter (isSuffixOf ".in") files
    mapM_ (correTeste tarefa4) inputs

correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    -- nome do ficheiro
    let nome = reverse $ drop 3 $ reverse input
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let o = outStr (tarefa (inStr inp))
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp) -- comparar resultados
    then putStrLn "Ok"
    else do
        putStrLn "Failed"
        putStrLn ""
        putStrLn "Expected output: "
        putStrLn esp
        putStrLn ""
        putStrLn "Received output: "
        putStrLn o
        putStrLn ""
