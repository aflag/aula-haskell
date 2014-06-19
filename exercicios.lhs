Introdução
==========

Estes são alguns exercícios que, acredito, irão ajudar na compreensão da
linguagem Haskell. Coloquei as soluções em um arquivo separado para não
dar nenhum spoiler, mas sinta-se livre para usá-las como preferir.

Este arquivo .lhs é um código literate haskell. Isto significa que, por
padrão, tudo que estiver escrito neste arquivo é comentário. Apenas
linhas que começam com > são códigos. Para carregar este arquivo, abra o
ghci e digite :l exercicios.lhs
    
    % ghci
    ghci> :l exercicios.lhs

Para dar reload no arquivo basta digitar :l exercicios.lhs novamente.

Eu uso o editor vim para programar em Haskell. Outro editor que também
suporta highlight de código Haskell é o editor Atom. Para isso, basta
instalar o pacote abaixo:

    https://atom.io/packages/language-haskell

(para instalar, vá em preferencias, selecione packages e o encontre lá)


Exercícios
==========

1. Haskell define uma função, last :: [a] -> a, que retorna o último elemento
de uma lista. Considere apenas o tipo dessa função. Quais são os possíveis
comportamentos que essa função pode ter (sem contar erros e loops infinitos)?
Dê exemplos de coisas que seria impossível que essa função fizesse.

2. Crie uma função safeHead usando o tipo Maybe que não cause erro no
caso de lista vazia. Tipo da função:

    safeHead :: [a] -> Maybe a

Definição de Maybe:

    data Maybe a = Just a | Nothing

3. Listas podem ser definidas da seguinte maneira:
    
> data List a = Cons a (List a) | Null deriving Show

Repare que, na definição de um tipo, os parâmetros dos contrutores são
tipos. Podendo, inclusive, ser o tipo que estamos definindo.

Com esse tipo, podemos criar a lista de inteiros [1,2,3] da seguinte
forma:

> list123 :: List Int
> list123 = Cons 1 (Cons 2 (Cons 3 Null))

Note como, no momento de usar, o construtor recebe por parâmetro
instâncias de um tipo. O construtor funciona igual uma função,
inclusive, podemos aferir o tipo dele como fazemos com funções:

    ghci> :t Cons
    Cons :: a -> List a -> List a

A função definida a seguir recebe uma função por parâmetro e a aplica a todos
elementos da lista:

> map' :: (a -> b) -> List a -> List b
> map' _ Null = Null
> map' f (Cons x xs) = Cons (f x) (map' f xs)

    ghci> map' (2+) list123
    Cons 3 (Cons 4 (Cons 5 Null))

Note que (2+) é uma aplicação parcial do parâmetro 2 à função +. [1]

Analise o tipo de map'. O que significa (a -> b)? O que significa List a
e List b?

Repare como a nossa lista não perde em nada para a lista padrão de
Haskell, que usa : ao invés de Cons e [] ao invés de Null.

4. Crie o tipo de dados árvore binária. Use deriving Show para que o
compilador crie automaticamente uma instancia de Show para sua árvore.

5. Vamos criar uma classe de tipos nova chamada Mapable. Qualquer tipo
de dado que pode ser iterado com uma função como map' deve instanciá-la:

> class Mappable m where
>     doMap :: (a -> b) -> m a -> m b

Instancie essa função para o tipo de dados de árvore criado.

6. Crie uma função que transforme a árvore em uma lista.

7. Use a função abaixo

> bind :: [a] -> (a -> [b]) -> [b]
> bind [] _ = []
> bind (x:xs) f = f x ++ (bind xs f)

Para fazer uma função que faz o produto cartesiano de [2,3,5] e [7,11].

    > cartesianProduct [2,3,5] [7,11]
    [(2,7),(2,11),(3,7),(3,11),(5,7),(5,11)]

Note que você provavelmente terá de chamar bind mais de uma vez.




[1] Em python, algo parecido seria:

    def add(x,y):
        return x+y
    map(functools.partial(add, 2), [1,2,3])
