{-|
Module      : Sokoban Tarefa 2
Description : Sokoban project for MIEI LI1 - Tarefa 2
License     : Unlicensed
-}

module Main where

import qualified Data.Text as T
import Data.List.Split
import Data.Char
-- |Função dada
inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]
-- |Função dada
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)
{- 
(1) Relativamente à simplificação do mapa:
     Seja x um elemento qualquer:
     Se na área de 1 unidade à sua volta não houver um único membro não-cardinal (ponto ou espaço), este é removido
     Caso contrário, ou seja, pelo menos um carater não-cardinal no raio de 1 unidade, este mantém-se.
     Primeiro de tudo há o problema de que, verificando as listas recursivamente, não é possível ver qual o valor atrás
     do membro atual. Como tal foram criadas duas funções. Uma que verifica normalmente e dá um valor 'B' ou 'V', dependendo
     dos parámetros, e outra que dá o valor final, revendo as linhas em sentido contrário, e a linha final é revertida uma última vez.
     Finalmente, é necessário tratar do caso especial da primeira e última linha, como tal é necessário
    apenas verificar a linha de baixo no caso da primeira, e de cima no caso da segunda. Como tal foi criada
    uma função que verifica apenas uma, e esta é aplicada na linha 1, e a mesma função em ordem contrária é aplicada
    na linha final. No caso de qualquer outras linhas (chamadas de torso ao longo deste código), verifica-se as linhas
    de cima e de baixo.
(2) Relativamente às coordenadas:
    Foi dividida a lista em head (coordenadas da personagem) e tail (coordenadas da caixa) e feitas as devidas funções.
    Como a coordenada (0,0) fazia respeito ao canto inferior esquerdo do mapa, decidi trabalhar com o reverse do mapa dado
    e reverter a lista final.
    Finalmente foi necessário colocar as coordenadas dadas no formato de listas de String, e passa-los para pares coordenados
    (Int,Int) com as devidas funções auxiliares, e importando <Data.List.Split>.
-}

main = do inp <- getContents
          putStr (outStr (tarefa2 (lines inp)))

          
-- |Função encarregue de simplificar o mapa e inserir coordenadas de personagem/caixas
tarefa2 :: [String] -> [String]
tarefa2 ficheiro = mapa_final

    where
    ficheiro_mapa = ficheiro 
    estrutura_mapa = take (numero_linhas '#' ficheiro_mapa) ficheiro_mapa             
    coordenadas = reverse ( take ( numero_coordenadas (ficheiro_mapa) ) (reverse ficheiro_mapa) ) 
    coordenadas_jogador = head coordenadas
    coordenadas_caixas = tail coordenadas
    linha1_unprocessed = confere_lista ((!!) estrutura_mapa 0) ((!!) estrutura_mapa 1)
    linha1 = reverse (rever_lista (reverse (linha1_unprocessed)) (reverse(((!!) estrutura_mapa 1))))
    linhaz_unprocessed = confere_lista (last (estrutura_mapa)) ((!!) estrutura_mapa ((length (estrutura_mapa)-2)))
    linhaz = reverse (rever_lista (reverse (linhaz_unprocessed)) (reverse (((!!) estrutura_mapa ((length (estrutura_mapa)-2))))))
    linhas_torso_unprocessed = map reverse(map confere_lista_torso (divide_lista_torso estrutura_mapa))
    linhas_torso_aplicavel = troca_segundo_lista (linhas_torso_unprocessed) ((reverse_lista (divide_lista_torso (estrutura_mapa))))
    linhas_torso = map reverse (map rever_lista_torso (linhas_torso_aplicavel))
    linhas_mapa_coordenadas = reverse estrutura_mapa
    mapa_simplificado = linha1 : (linhas_torso ++ [linhaz])
    mapa_heroi = let n = (string_list coordenadas_jogador) 
                            in reverse (insere_personagem_linha ((read((!!) n 0)),(read((!!) n 1))) (reverse mapa_simplificado) )
    mapa_final = let x = map (splitOn [' ']) (coordenadas_caixas) --  Torna a lista de coordenadas "legível" para a função
                            in reverse (insere_todas_caixas (parifica x) (reverse mapa_heroi)) --  Lista do output final                    
--  /Funções auxiliares para o tratamento do ficheiro
--  Relativas ao mapa
-- |Dá o número de linhas que contém apenas o mapa
numero_linhas :: Char -> [String] -> Int
numero_linhas a [] = 0
numero_linhas a (x:xs)
    | a == (head x) = 1 + numero_linhas a xs
    | otherwise = 0
-- |Dá o número de linhas que contém apenas as coordenadas
numero_coordenadas :: [String] -> Int 
numero_coordenadas [] = 0
numero_coordenadas (x:xs)
    | isDigit (head x) == True = 1 + numero_coordenadas xs
    | otherwise = numero_coordenadas xs
-- |Divide o mapa numa lista de listas com 3 membros cada, para faciliar a função <confere_lista>
divide_lista_torso :: [String] -> [[String]]
divide_lista_torso [] = []
divide_lista_torso [x] = []
divide_lista_torso [x,y,z] = [[x,y,z]]
divide_lista_torso (x:x1:x2:xs) =[x,x1,x2] : divide_lista_torso (x1:x2:xs)
-- |Coloca todos os elementos da lista a1 e troca-os com todo o 2º elemento de conjunto de listas de a2
troca_segundo_lista :: [[a]] -> [[[a]]] -> [[[a]]] 
troca_segundo_lista [] [] = []
troca_segundo_lista (x:xs) ((y:ys:yz):z) = (y:x:yz) : troca_segundo_lista xs z
-- |reverse_lista [["123","456","789"],["abc","def","ghi"]] -> [["321","654","987"],["cba","fed","ihg"]]
reverse_lista :: [[[a]]] -> [[[a]]]
reverse_lista [] = []
reverse_lista ((x:xs):y) = ( map reverse (x:xs) ) : reverse_lista y
-- Relativas ao mapa/
-- Relativas às coordenadas
-- |Função auxiliar para ordenar coordenadas (1)
min_coordenadas :: Ord a => Ord b => (a,b) -> (a,b) -> (a,b)
min_coordenadas (x,xs) (y,ys)
    | x < y = (x,xs)
    | x > y = (y,ys)
    | x == y = if xs > ys then (y,ys) else (x,xs)
-- |Função auxiliar para ordenar coordenadas (2)
minimum_coordenadas :: Ord a => Ord b => [(a,b)] -> (a,b)
minimum_coordenadas [(x,y)] = (x,y)
minimum_coordenadas (x:y) = min_coordenadas x (minimum_coordenadas y)
-- |Função auxiliar para ordenar coordenadas (3)
delete' :: Eq a => a -> [a] -> [a] 
delete' _ [] = []
delete' n (x:xs)
    | n == x = xs
    | otherwise = x : delete' n xs
-- |string_list ("1 22") -> [1,22]
string_list :: String -> [String] 
string_list [] = []
string_list (x:xs) = splitOn [' '] (x:xs)
-- |Torna as coordenadas na forma de lista de pares, para haver concordância com outras funções
parifica :: [[String]] -> [(Int,Int)]
parifica [] = []
parifica ((x:xs):ys) = (read x,read(head xs)) : parifica ys
--  Relativamente às coordenadas/
--  Fim das funções auxiliares/
--  /Funções relativas à construção do mapa
-- |Aplicável apenas à primeira e última linha do mapa, por serem casos especiais
confere_lista :: String -> String -> String
confere_lista [] [] = []
confere_lista [a] [b] = "V" --  V = "Veremos o seu valor na próxima função"(função que re-vê as linhas do ângulo contrário)
confere_lista (x:xs) (y:ys)
    | or [y /= '#',head ys /= '#'] = 'B' : confere_lista xs ys --  B = "Bom". Ou seja, o seu valor será automaticamente '#' na função de revisão
    | and [x == '#',y == '#', head ys == '#'] = 'V' : confere_lista xs ys
    | otherwise = 'V' : confere_lista (xs) (ys)
-- |Função que re-vê as linhas pelo lado contrário, apartir dos valores 'B' e 'V' atribuidos    
rever_lista :: String -> String -> String
rever_lista [] [] = []
rever_lista [a] [b] = case a of 'B' -> "#"
                                'V' -> if b == '#' then " "
                                          else " "        
rever_lista (x:xs) (y:ys) = case x of 'B' -> '#' : rever_lista xs ys
                                      'V' -> if (or [y /= '#', head ys /= '#']) then '#' : rever_lista xs ys
                                             else ' ' : rever_lista xs ys
-- |Mesmo processo para as linhas intermédias do mapa, neste caso a função verifica as linhas em cima e em baixo da linha aplicada                                             
confere_lista_torso :: [String] -> String 
confere_lista_torso [] = []
confere_lista_torso [[x],[y],[z]]
    | and [x == y, y == z] = "V"
    | otherwise = "V"
confere_lista_torso [(x:xs),(y:ys),(z:zs)] = case y of '#' -> if or ([x == ' ', head xs == ' ', z == ' ', head zs == ' ', head ys == ' ',x == '.', head xs == '.', z == '.', head zs == '.', head ys == '.',x == 'I', head xs == 'I', z == 'I', head zs == 'I', head ys == 'I',x == 'H', head xs == 'H', z == 'H', head zs == 'H', head ys == 'H',x == 'o', head xs == 'o', z == 'o', head zs == 'o', head ys == 'o'])
                                                                then 'B' : confere_lista_torso [xs,ys,zs]
                                                                else 'V' : confere_lista_torso [xs,ys,zs]
                                                       ' ' -> ' ' : confere_lista_torso [xs,ys,zs]
                                                       '.' -> '.' : confere_lista_torso [xs,ys,zs]
-- |Mesmo sistema a função rever_lista, mas neste caso é necessário verificar ambas a lista de cima e de baixo                                                   
rever_lista_torso :: [String] -> String 
rever_lista_torso [] = []
rever_lista_torso [[x],[y],[z]] = case y of 'B' -> "#"
                                            ' ' -> " "
                                            '.' -> "."
                                            'V' -> if and [x == y, y ==z] then "#"
                                                   else " "
rever_lista_torso [(x:xs),(y:ys),(z:zs)] = case y of 'B' -> '#' : rever_lista_torso [xs,ys,zs]
                                                     'V' -> if or ([x == ' ', head xs == ' ', z == ' ', head zs == ' ', head ys == ' ',x == '.', head xs == '.', z == '.', head zs == '.', head ys == '.',x == 'I', head xs == 'I', z == 'I', head zs == 'I', head ys == 'I',x == 'H', head xs == 'H', z == 'H', head zs == 'H', head ys == 'H',x == 'o', head xs == 'o', z == 'o', head zs == 'o', head ys == 'o'])
                                                                then '#' : rever_lista_torso [xs,ys,zs]
                                                                else ' ' : rever_lista_torso [xs,ys,zs]
                                                     ' ' -> ' ' : rever_lista_torso [xs,ys,zs]
                                                     '.' -> '.' : rever_lista_torso [xs,ys,zs]
--  Funções relativas à contrução do mapa/
--  Funções relativas ao inserir de coordenadas
-- |Coloca as coordenadas por ordem crescente de valor x em (x,y), para facilitar a utilização de outras funções
ordenar_coordenadas :: Ord a => Ord b => [(a,b)] -> [(a,b)]
ordenar_coordenadas [] = []
ordenar_coordenadas ((x,xs):ys) = let n = ((x,xs):ys) in if (x,xs) == minimum_coordenadas n 
                                                         then (x,xs) : ordenar_coordenadas ys
                                                         else (minimum_coordenadas n) : ordenar_coordenadas (delete' (minimum_coordenadas n) n) 
-- |Função procura inicialmente em qual linha deverá inserir a personagem
insere_personagem_linha :: (Int,Int) -> [[Char]] -> [[Char]] 
insere_personagem_linha (_,_) [] = []
insere_personagem_linha (a,0) ((x:xs):y) = (insere_personagem_coluna (x:xs) a) : y
insere_personagem_linha (a,b) ((x:xs):y) = (x:xs) : insere_personagem_linha (a,(b-1)) y
-- |Partindo do princípio que a linha já foi escolhida, função encarrega-se de inserir a personagem na coluna correta
insere_personagem_coluna :: [Char] -> Int -> [Char]
insere_personagem_coluna [] _ = []
insere_personagem_coluna (x:xs) 0 = 'o' : xs 
insere_personagem_coluna (x:xs) n = x : ( insere_personagem_coluna xs (n-1) )
-- |Análogo a <inserir_personagem_linha>
insere_caixa_linha :: (Int,Int) -> [[Char]] -> [[Char]]
insere_caixa_linha (_,_) [] = []
insere_caixa_linha (a,0) ((x:xs):y) = (insere_caixa_coluna (x:xs) a ) : y
insere_caixa_linha (a,b) ((x:xs):y) = (x:xs) : insere_caixa_linha (a,(b-1)) y
-- |Análogo a <inserir_personagem_coluna>, exceto com a necessidade de avaliar o valor de x
insere_caixa_coluna :: [Char] -> Int -> [Char]
insere_caixa_coluna [] _ = []
insere_caixa_coluna (x:xs) 0
    | x == ' ' = 'H' : xs --  Tratando-se de um espaço vazio
    | x == '.' = 'I' : xs --  Tratando-se de um ponto de entrada
    | otherwise = '#' : xs --  Caso as coordenadas sejam invalidas. Trata-se de uma linha redundante, admitindo que o input de Tarefa2 é válido
insere_caixa_coluna (x:xs) n = x : insere_caixa_coluna xs (n-1)
-- |Função que insere todas as caixas no mapa
insere_todas_caixas :: [(Int,Int)] -> [String] -> [String] 
insere_todas_caixas [] [] = []
insere_todas_caixas [(x,xs)] (y:ys) = insere_caixa_linha (x,xs) (y:ys)
insere_todas_caixas ((x,xs):z) (y:ys) = insere_todas_caixas z (insere_caixa_linha (x,xs) (y:ys))
--  Funções relativas ao inserir de coordenadas/ 