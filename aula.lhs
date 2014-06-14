> import Control.Exception

Usei os livros:

http://learnyouahaskell.com/
http://book.realworldhaskell.org/

Ambos disponíveis online.


Operadores
==========

ghci> 2 + 15
17
ghci> 49 * 100
4900
ghci> 1892 - 1472
420
ghci> 5 / 2
2.5

ghci> True && False 
False 
ghci> True && True 
True 
ghci> False || True 
True  
ghci> not False 
True 
ghci> not (True && True) 
False

O diferente é /= e não !=

ghci> 5 == 5 
True 
ghci> 1 == 0 
False 
ghci> 5 /= 5 
False 
ghci> 5 /= 4 
True 
ghci> "hello" == "hello" 
True


Definição de função
===================

Funções são definidas da seguinte maneira:

> doubleMe x = x + x

Esta função dobra o número passado por parâmetro. Como podemos fazer uma função
que quadriplica um número?

> quadrupleMe = doubleMe . doubleMe

E para dobrar apenas números menores que 100?

> doubleSmall x = if x > 100
>                    then x
>                    else 2*x

O if é uma expressão. O else é sempre obrigatório. Para somar mais um ao
resultado da expressão anterior poderíamos fazer:

> doubleSmall' x = (if x > 100 then x else 2*x) + 1

O valor do if-then-else é o primeiro valor se x > 100 e o segundo caso
contrário.


Trabalhando com listas
======================

As listas em Haskell são criadas de forma parecida com as listas em Python. Por
exemplo:

> arbitraryList = [4, 2, 5, 7, 13, 88]

Uma lista pode ter apenas valores do mesmo tipo. A lista

    [1, 'a', "Oi"]

é inválida. Note que 'a' é um caractere e "Oi" uma string.

Uma String é uma lista de caracteres

    ghci> ['h', 'e', 'l', 'l', 'o'] ++ " world"
    "hello world"
    ghci> ['h', 'e', 'l', 'l', 'o'] == "hello"
    True

Concatenação de listas pode ser feita com o operador ++.

    ghci> arbitraryList ++ [1..7]
    [4,2,5,7,13,88,1,2,3,4,5,6,7]

Concatenar com ++ é uma operação linear. Podemos só colocar um elemento na
frente da lista (não precisa percorrer a lista toda):

    ghci> 1:[2,3,4]
    [1,2,3,4]

Na verdade, [1,2,3,4] é syntax sugar para 1:2:3:4:[].

Algumas funções úteis para listas:

    ghci> head [5,3,2,7]
    5
    ghci> tail [5,3,2,7]
    [3,2,7]
    ghci> last [5,3,2,7]
    7
    ghci> init [5,4,2,7]
    [5,4,2]

http://s3.amazonaws.com/lyah/listmonster.png

Todas essas 4 funções falham com uma lista vazia:

    ghci> head []
    *** Exception: Prelude.head: empty list

    ghci> length [5,4,3,2,1]
    5
    ghci> null [1,2,3] 
    False 
    ghci> null [] 
    True 
    ghci> reverse [5,4,3,2,1]
    [1,2,3,4,5]
    ghci> elem 7 [1..7]
    True
    ghci> elem 8 [1..7]
    False

List comprehensions parecem nossas definições de conjuntos em matemática:

    ghci> [x*2 | x <- [1..10]]
    [2,4,6,8,10,12,14,16,18,20]

Gera uma lista de ímpares entre 1 e 10:

    ghci> [x | x <- [1..10], x `mod` 2 /= 0]
    [1,3,5,7,9]

mod é uma função, mas `` transforma qualquer função em um infixo.


Mais funções!
=============

Pattern matching!

> lucky :: Int -> String
> lucky 7 = "LUCKY NUMBER SEVEN!"
> lucky x = "Sorry, you're out of luck, pal!"

Caso o número case com 7 (ou seja, se o número é 7), então usa a primeira
definição. Senão a segunda definição será utilizada.

Pattern matching não precisa incluir todos casos

> ops :: Int -> Int
> ops 7 = 5

    ghci> ops 2
    *** Exception: aula.lhs:173:3-11: Non-exhaustive patterns in function ops

Também funciona para listas:

> firstIfListOfTwo :: [Int] -> Int
> firstIfListOfTwo [x,y] = x
> firstIfListOfTwo list = 0

Nosso head

> head' :: [a] -> a
> head' (x:_) = x

Recursão!

A boa e velha função fatorial

> factorial :: Int -> Int
> factorial 0 = 1
> factorial n = n * factorial (n-1)

Vamos agora iterar sobre uma lista e criar uma função que soma todos valores da
lista

> sum' :: [Int] -> Int
> sum' [] = 0
> sum' (x:xs) = x + sum' xs

Outras formas de fazer a mesma função:

Sem pattern matching

> sum'' :: [Int] -> Int
> sum'' xs = if null xs
>                then 0
>                else (head xs) + sum'' (tail xs)

Usando guards ao invés de if-then-else

> sum''' :: [Int] -> Int
> sum''' xs
>     | null xs = 0
>     | otherwise = head xs + sum'' (tail xs)

Uso de where (e listas infinitas)

> isPerfectSquare :: Int -> Bool
> isPerfectSquare n = intSqrt n * intSqrt n == n
>     where intSqrt = floor . sqrt . fromIntegral
>
> perfectSquares :: [Int]
> perfectSquares = [x | x <- [1..], isPerfectSquare x]

Criamos uma lista de infinitas raízes perfeitas. O que acontece se checarmos a
posição número 2000 a lista 2 vezes?


Entrada e saída
===============

Uma explicação do caminho de Haskell:

https://www.youtube.com/watch?v=iSmkqocn0oQ

O caminho de Haskell foi diferente das outras linguagens. Ela começou
completamente pura e tem caminhado para colocar efeito colateral na linguagem
sem perder sua pureza. Fazer IO e continuar pura foi um grande trabalho de
pesquisa dos desenvolvedores da linguagem.

O tipo da função main é

    main :: IO ()

estamos dentro da monad IO. Vamos arriscar um Hello world:

> helloWorld = putStrLn "hello world!"

Mas a gente muitas vezes quer criar uma série de passos (programação
imperativa). Vamos fazer programação imperativa em Haskell!

> displayPerfectSquare = do
>     putStrLn "Qual raíz perfeita você quer?"
>     line <- getLine
>     putStrLn (show (perfectSquares !! read line))
>     displayPerfectSquare

E se a pessoa digitar um número inválido?

> displayPerfectSquare' = do
>     putStrLn "Qual raíz perfeita você quer?"
>     line <- getLine
>     result <- try (evaluate (read line)) :: IO (Either ErrorCall Int)
>     case result of
>         Left _ -> putStrLn "Número inválido"
>         Right index -> putStrLn (show (perfectSquares !! index))
>     displayPerfectSquare


Tipos de dados
==============

Vamos fazer funções para registrar quem está em um prédio. Uma pessoa pode ser
um funcionário ou um visitante. O visitante tem um nome e uma matricula. O
visitante, apenas o nome.

> data Person = Employee String Int | Visitor String

Como encontraríamos um empregado por um número de matrícula?

> findEmployeeByRegistration :: Int -> [Person] -> Person
> findEmployeeByRegistration registration ((Employee name r):xs) =
>     if r == registration
>         then Employee name r
>         else findEmployeeByRegistration registration xs
> findEmployeeByRegistration registration ((Visitor _):xs) = findEmployeeByRegistration registration xs

Mas quando a gente tenta encontrar um empregado no interpretador as coisas não
funcionam:

    ghci> findEmployeeByRegistration 2027 [Visitor "Joana", Employee "Marcos" 2027]
    <interactive>:141:1:
        No instance for (Show Person)
          arising from a use of `print'
        Possible fix: add an instance declaration for (Show Person)
        In a stmt of an interactive GHCi command: print it

O que acontece é que o interpretador não sabe como transformar o nosso tipo em
uma String. Para fazer isso o interpretador usa a função show:

    ghci> :t show
    show :: Show a => a -> String

Mas nosso tipo não é uma instancia de Show. Vamos corrigir isso:

> instance Show Person where
>     show (Visitor name) = "Visitante (Nome: " ++ name ++ ")"
>     show (Employee name registration) = "Empregado (Nome: " ++ name ++ ", Matrícula: " ++ show registration ++ ")"

    ghci> findEmployeeByRegistration 2027 [Visitor "Joana", Employee "Marcos" 2027]
    Empregado
        Nome: Marcos
        Matrícula: 2027

Mas e se não existir o empregado com a matrícula que a gente quer? E se a lista
for vazia?

    ghci> findEmployeeByRegistration 2027 [Visitor "Joana", Employee "Marcos" 202]
    *** Exception: aula.lhs:(281,3)-(285,103): Non-exhaustive patterns in function findEmployeeByRegistration
    ghci> findEmployeeByRegistration 2027 []
    *** Exception: aula.lhs:(281,3)-(285,103): Non-exhaustive patterns in function findEmployeeByRegistration

O que retornar nesse caso? Vamos deixar esse erro? Lembra como era difícil
resolver isso no código com efeitos colaterais?

Vamos usar um outro tipo chamado Maybe. A definição do Maybe é assim:

    data Maybe a = Just a | Nothing

Vamos refazer nossa função agora.

> findEmployeeByRegistration' :: Int -> [Person] -> Maybe Person
> findEmployeeByRegistration' registration ((Employee name r):xs) =
>     if r == registration
>         then Just (Employee name r)
>         else findEmployeeByRegistration' registration xs
> findEmployeeByRegistration' registration ((Visitor _):xs) = findEmployeeByRegistration' registration xs
> findEmployeeByRegistration' _ [] = Nothing
