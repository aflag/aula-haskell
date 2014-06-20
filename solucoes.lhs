Introdução
==========

Estas são as resoluções que eu dei para os problemas propostos. Não são,
necessariamente, as melhores ou mesmo as mais corretas. Este arquivo não
é um gabarito, mas apenas uma sugestão :)

Este arquivo .lhs é um código literate haskell. Isto significa que, por
padrão, tudo que estiver escrito neste arquivo é comentário. Apenas
linhas que começam com > são códigos. Para carregar este arquivo, abra o
ghci e digite :l solucoes.lhs
    
    % ghci
    ghci> :l solucoes.lhs

Para dar reload no arquivo basta digitar :l solucoes.lhs novamente.


Soluções
========

1. Essa função pode retornar qualquer elemento da lista, pois todos eles
tem os mesmo tipo: a. Seria difícil, talvez impossível, retornar um
objeto que não esteja da lista, pois essa função precisaria retornar um
objeto do tipo certo, independente do tipo contido na lista. Por
exemplo, a função

    last [] = 5
    last (_:_) = 5

teria o tipo last :: Num a => [b] -> a, mas como poderíamos remover o
(Num a) dessa assinatura sem usar um elemento da lista?

Procurando no hoogle por funções com tipo a -> a também não encontramos
nenhuma função que poderíamos usar para modificar um valor genérico na
lista. Portanto, uma função com assinatura [a] -> a só tem como opção
retornar um elemento da lista. Não necessariamente o último.

2.

> safeHead :: [a] -> Maybe a
> safeHead [] = Nothing
> safeHead (x:_) = Just x

3. O construtor Cons e : funcionam da mesma forma. Exceto que Cons é
sufixado e : infixado. Os construtores [] e Null também tem o mesmo
funcionamento. Exceto por possíveis otimizações que o compilador pode
vir a fazer -- tratando listas de forma especial -- ambos tipos
comportam-se da mesma forma.

Na sintaxe de definição de tipos em Haskell a setinha (->) significa uma
função. Toda função recebe um parametro e retorna um resultado.
Portanto, o tipo

    f :: Int -> Float -> String -> [String]

também pode ser escrito como

    f :: Int -> (Float -> (String -> [String]))

Ou seja, f é uma função que recebe um Int. Ela retorna uma fução que
recebe um Float. Essa função retornada, por sua vez, recebe uma String e
retorna uma lista de String.

Já o tipo de map' é:

    map' :: (a -> b) -> List a -> List b

que pode ser escrito também

    map' :: (a -> b) -> (List a -> List b)

Ao colocar os parenteses em (a -> b) estamos falando que map' é uma
função que recebe uma outra função por parâmetro. Essa função recebe um
tipo qualquer e retorna um outro tipo qualquer (que pode, inclusive, ser
igual a). Além disso, map' retorna uma função que recebe uma lista de as
e retorna uma lista de bs.

Pelos mesmos motivos já descritos no exercício anterior, é difícil
imaginar uma definição válida de map' que não envolva usar a função
recebida como parametro para modificar a lista de as para criar a nova
lista de bs. Note que, mesmo aplicar a função recebida por parâmetro
duas vezes seria um problema. Se o tipo de map' fosse

    map'' :: (Int -> Int) -> List Int -> List Int

haveria várias formas de implementar essa função sem usar a função
recebida por parâmetro. Por exemplo

    map'' _ = map' (1+)

Porém, devido a generalidade dos tipos, a única forma de implementar
map' de forma incorreta parece ser aplicando a função apenas a um pedaço
da lista ou retornando uma lista vazia (Null).

A lista é um container que pode guardar qualquer tipos de objetos. Ao
manter a tipagem do conteúdo da lista genérico, nós garantimos que
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
utilizando infix order.

> treeToList :: BinaryTree a -> [a]
> treeToList Null = []
> treeToList (Node x left right) = (treeToList left) ++ [x] ++ (treeToList right)

7. A função passada por parâmetro deve transformar cada elemento da lista [a]
em uma lista. As listas geradas pela função são concatenadas.

Minha ideia é chamar bind com a lista [2,3,5]. Para cada elemento da lista, nós
chamamos bind de novo com a lista [7,11]. No final, isso fará com que, para
cada elemento de [2,3,5], percorramos a lista [7,11].

> bind :: [a] -> (a -> [b]) -> [b]
> bind [] _ = []
> bind (x:xs) f = f x ++ (bind xs f)

> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct l1 l2 = bind l1 (\x -> bind l2 (\y -> [(x,y)]))

    ghci> cartesianProduct [2,3,5] [7,11]
    [(2,7),(2,11),(3,7),(3,11),(5,7),(5,11)]

Quando estivermos processando 2, o segundo bind irá gerar

    [(2,7)] ++ [(2,11)]
    [(2,7), (2,11)]

O primeiro bind irá concatenar essa lista à lista

    [(3,7), (3,11)]

e

    [(5,7), (5,11)]

No final, obteremos nosso produto cartesiano.

De onde surgiu essa função bind? Essa é a função mais importante na definição
de uma monad. Em haskell, a função bind é representada por >>=

Reescrevendo o nosso produto cartesiano usando >>= fica assim:

> cartesianProduct' :: [a] -> [b] -> [(a,b)]
> cartesianProduct' l1 l2 = l1 >>= (\x -> l2 >>= (\y -> [(x,y)]))

Existe um syntax sugar em haskell para escrever essa mesma expressão usando a
notação do:

> cartesianProduct'' :: [a] -> [b] -> [(a,b)]
> cartesianProduct'' l1 l2 = do
>     x <- l1
>     y <- l2
>     [(x,y)]

Implementamos a função bind igual a monad [] implementa a >>= dela. Existem
várias Monads em Haskell, além da IO monad.

Este é apenas nosso primeiro contato com Monads. Mas já é possível ver como a
coisa é poderosa.
