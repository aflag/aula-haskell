Introdução
==========

Estas são as resoluções que eu dei para os problemas propostos. Não são,
necessariamente, as melhores ou mesmo as mais corretas. Este arquivo não
é um gabarito, mas apenas uma sugestão :)

Este arquivo .lhs é um código literate Haskell. Isto significa que, por
padrão, tudo que estiver escrito neste arquivo é comentário. Apenas
linhas que começam com > são código. Para carregar este arquivo, abra o
ghci e digite :l solucoes.lhs

    % ghci
    ghci> :l solucoes.lhs

Para fazer reload do arquivo basta digitar :l solucoes.lhs novamente.


Soluções
========

1. A função last pode retornar qualquer elemento da lista, pois todos
eles tem os mesmo tipo: a. Seria difícil, talvez impossível, retornar um
objeto que não esteja da lista. Afinal, essa função precisa retornar um
objeto de um tipo arbitrário, igual ao que está contido na lista. Por
exemplo, a função

    last [] = 5
    last (_:_) = 5

é do tipo last :: Num a => [b] -> a. Como poderíamos retornar um
valor do tipo b sem que ele seja um dos elementos da lista? Como
construir esse valor?

Procurando no Hoogle por funções com tipo a -> a não encontro nenhuma
função que modifique um tipo genérico.  Portanto, uma função com
assinatura [a] -> a só tem como opção retornar um elemento da lista. Não
necessariamente o último.

2.

> safeHead :: [a] -> Maybe a
> safeHead [] = Nothing
> safeHead (x:_) = Just x

3. O construtor Cons e : funcionam da mesma forma. A diferença é que
Cons é sufixado e : infixado. Os construtores [] e Null também tem o
mesmo funcionamento. Os tipos [a] e List a funcionam da mesma forma.

Na sintaxe de definição de tipos em Haskell a seta (->) significa uma
função. Toda função recebe um parâmetro e retorna um resultado.
O tipo

    f :: Int -> Float -> String -> [String]

também pode ser escrito como

    f :: Int -> (Float -> (String -> [String]))

Ou seja, f é uma função que recebe um Int e retorna uma função. Essa
função recebe um Float e retorna uma nova função. Essa função retornada,
por sua vez, recebe uma String e retorna uma lista de Strings.

Já o tipo de map' é:

    map' :: (a -> b) -> List a -> List b

que pode ser escrito como

    map' :: (a -> b) -> (List a -> List b)

Ao colocar os parenteses em (a -> b) estamos falando que map' é uma
função que recebe uma outra função por parâmetro. (a -> b) é o parâmetro
da função. Além disso, map' retorna uma função que recebe uma lista de
as e retorna uma lista de bs.

A função que map' recebe por parâmetro, (a -> b) é uma função que pega
um tipo arbitrário a e retorna outro tipo arbitrário b. Inclusive, a
pode ser igual a b.

Pelos mesmos motivos já descritos no exercício anterior, é difícil
imaginar uma definição válida de map' que não envolva usar a função
recebida como parâmetro para modificar a lista. Note que, mesmo aplicar
a função recebida por parâmetro duas vezes seria um problema, pois os
tipos do parâmetro e retorno da função são diferentes.

Se o tipo de map' fosse

    map'' :: (Int -> Int) -> List Int -> List Int

haveria várias formas de implementar essa função sem usar a função
recebida por parâmetro. Por exemplo

    map'' _ = map' (1+)

Porém, devido a generalidade dos tipos, a única forma de implementar
map' de forma incorreta parece ser aplicando a função apenas a um pedaço
da lista ou retornando uma lista vazia (Null).

A lista é um container que pode guardar qualquer tipos de objetos. Ao
manter o tipo do conteúdo da lista genérico, nós garantimos que
nossas funções que operam sobre o container, como map, tenham efeito
apenas no container e não no conteúdo dele.

4.

> data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Null deriving Show

5.

> class Mappable m where
>     doMap :: (a -> b) -> m a -> m b

> instance Mappable BinaryTree where
>   doMap _ Null = Null
>   doMap f (Node x left right) = Node (f x) (doMap f left) (doMap f right)

6. Existem mais de uma forma de transformar uma árvore binária em uma
lista. Neste exercício vou transformar uma árvore binária em uma lista
fazendo um caminhamento em ordem. Qualquer caminhamento é uma solução
válida do exercício.

> treeToList :: BinaryTree a -> [a]
> treeToList Null = []
> treeToList (Node x left right) = (treeToList left) ++ [x] ++ (treeToList right)

7. A função passada no parâmetro de bind deve transformar cada elemento
da lista [a] em uma lista. As listas geradas pela função são
concatenadas.

Minha ideia é chamar bind com a lista [2,3,5]. Para cada elemento da
lista, nós chamamos bind de novo com a lista [7,11]. No final, isso fará
com que, para cada elemento de [2,3,5], percorramos a lista [7,11].

A propósito, a sintaxe

    \x y -> x+y

significa que estamos criando uma função lambda que recebe dois
parâmetros e retorna sua soma. Isso é análogo à construção de Python:

    lambda x, y: x+y

Agora vamos à definição da função:

> bind :: [a] -> (a -> [b]) -> [b]
> bind [] _ = []
> bind (x:xs) f = f x ++ (bind xs f)

> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct l1 l2 = bind l1 (\x -> bind l2 (\y -> [(x,y)]))

    ghci> cartesianProduct [2,3,5] [7,11]
    [(2,7),(2,11),(3,7),(3,11),(5,7),(5,11)]

Quando x=2 (o bind mais externo irá chamar a função lambda com esse
valor), o bind dentro da função lambda, com auxilio do lambda que recebe
y, irá fazer o seguinte:

    [(2,7)] ++ [(2,11)]
    [(2,7), (2,11)]

O primeiro bind irá concatenar essa lista às listas

x=3
    [(3,7), (3,11)]

x=5
    [(5,7), (5,11)]

No final, obteremos nosso produto cartesiano.

De onde surgiu essa função bind? Essa é a função mais importante na definição
de uma Monad. Em Haskell, a função bind é representada por >>=

Reescrevendo o nosso produto cartesiano usando >>= fica assim:

> cartesianProduct' :: [a] -> [b] -> [(a,b)]
> cartesianProduct' l1 l2 = l1 >>= (\x -> l2 >>= (\y -> [(x,y)]))

Existe um syntax sugar em Haskell para escrever essa mesma expressão usando a
notação do:

> cartesianProduct'' :: [a] -> [b] -> [(a,b)]
> cartesianProduct'' l1 l2 = do
>     x <- l1
>     y <- l2
>     return (x,y)

Implementamos a função bind igual a Monad [] (a instanciação de Monad
pela lista) implementa a >>= dela. Existem várias Monads em Haskell,
além da IO Monad. A lista é uma Monad, como já vimos, Maybe também é,
String, como é uma lista, também é uma Monad.

O interessante dessa Monad de lista é que nós podemos escrever um
algoritmo não determinístico como se fosse uma lista de passos
determinísticos. Cada Monad irá criar um contexto de computação que irá
nos auxiliar a fazer determinados tipos de algoritmos.
