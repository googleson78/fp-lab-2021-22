# Бележка за обхождане
Когато работим с дървета има много начини по които можем да ги обхождаме.

Нека изберем "ляво корен дясно" за всички задачи по-долу

Пример:
```haskell
> treeToList (Node 2 (Node 4 (Node 5 Empty Empty) Empty) (Node 7 Empty Empty))
[5,4,2,7]
```
Има тестове, които зависят от това (например тестът който проверява че правенето
на двоично наредено дърво сортира списък).

## 00. Равенство на дървета
```haskell
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
```

Имплементирайте "структурно" равенство за дървета.

Примери:
```haskell
> Empty == Empty
True

> Empty == Node 2 Empty Empty
False

> Node 2 Empty Empty == Node 3 Empty Empty
False

> Node 2 (Node 4 Empty Empty) Empty == Node 2 (Node 4 Empty Empty) Empty
True
```

## 01. Двоични наредени дървета
Ще имплементираме някои функционалности на [двоично наредено дърво](https://en.wikipedia.org/wiki/Binary_search_tree).

### 01.00. Вмъкване
```haskell
insertOrdered :: Ord a => a -> Tree a -> Tree a
```

Напишете функция, която вмъква елемент в дърво, при презумцията, че то е
двоично наредено дърво.

При вмъкване на стойност, която вече присъства в дървото, се приема че
тя се вмъква втори път.

Искаме резултатното дърво също да е двоично наредено дърво.

Примери:
```haskell
> insertOrdered 5 Empty
Node 5 Empty Empty

> insertOrdered 5 (Node 7 Empty Empty)
Node 7 (Node 5 Empty Empty) Empty

> insertOrdered 10 (Node 7 Empty Empty)
Node 7 Empty (Node 10 Empty Empty)

> insertOrdered 7 (Node 8 (Node 6 Empty Empty) Empty)
Node 8 (Node 6 Empty (Node 7 Empty Empty)) Empty

> insertOrdered 7 (Node 8 (Node 6 Empty (Node 7 Empty Empty)) Empty)
Node 8 (Node 6 Empty (Node 7 (Node 7 Empty Empty) Empty)) Empty
```

### 01.01. Строене от списък
```haskell
listToBST :: Ord a => [a] -> Tree a
```

Построете двоично наредено дърво по подаден списък.

N.B.: Не е нужно резултатът ви да е точно както примерите,
а само да е двоично наредено дърво.

Примери:
```haskell
> listToBST []
Empty

> listToBST [1,2,3]
Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty

> listToBST [2,1,3]
Node 3 (Node 1 Empty (Node 2 Empty Empty)) Empty

> listToBST [0..10]
Node 10 (Node 9 (Node 8 (Node 7 (Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 (Node 0 Empty Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty

> listToBST [5,4,6,3,7,2,8,1,9,0,10]
Node 10 (Node 0 Empty (Node 9 (Node 1 Empty (Node 8 (Node 2 Empty (Node 7 (Node 3 Empty (Node 6 (Node 4 Empty (Node 5 Empty Empty)) Empty)) Empty)) Empty)) Empty)) Empty
```

### 01.02. Проверка за двоично наредено дърво
```haskell
isBST :: Ord a => Tree a -> Bool
```

Проверете дали дърво е двоично наредено дърво.

Примери:
```haskell
> isBST $ Node 8 (Node 6 Empty (Node 7 Empty Empty)) Empty
True

> isBST Empty
True

> isBST $ Node 8 (Node 6 Empty (Node 7 Empty Empty)) Empty
True

> isBST $ Node 8 (Node 6 Empty Empty) Empty
True

> isBST $ listToBST [0..10]
True

> isBST $ listToBST [0..1000]
True
```
**(яка) Идея за имплементация:**
```haskell
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)
```
Със следния тип ще изразяваме наредбата на `a`, но с добавени
"най-малък" (`Bot`) и "най-голям" (`Top`) елементи.

(това е точно автоматично генерираната инстанция за `Ord`)

Примери:
```haskell
> Bot <= Top
True

> Top <= Bot
False

> Bot <= Val 0
True

> Top <= Val 0
False

> Bot <= Val (minBound :: Int)
True

> Top <= Val (maxBound :: Int)
False
```

Имплементирайте рекурсивно следната функция:
```haskell
between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
```

с идеята че тя ще проверява дали двоично наредено дърво е между подадените граници.

След това `isBST` е просто
```haskell
isBST = between Bot Top
```

Примери:
```haskell
> between (Val 0) (Val 10) $ Node 8 (Node 6 Empty Empty) Empty
True

> between (Val 0) (Val 5) $ Node 8 (Node 6 Empty Empty) Empty
False

> between (Val 7) (Val 10) $ Node 8 (Node 6 Empty Empty) Empty
False
```
**HINT (don't read straight away)** за `between`:

Идеята ни за `between` е че всеки път когато слизаме в ляво и дясно поддърво,
искаме да "обновяваме" изискванията ни за границите им, взимайки предвид
елемента в сегашния връх и дали влизаме в лявото или в дясното поддърво.

Празното дърво е между каквито и да са граници (защото то няма елементи),
стига границите да са "валидни" - лявата да е по-малка или равна на дясната.

### 01.03. Търсене в двоично наредено дърво
```haskell
findBST :: Ord a => a -> Tree a -> Bool
```

Намерете дали присъства елемент в дърво, възползвайки се от това че то е двойчно наредено дърво,
с цел да имаме логаритмично търсене, ако дървото е балансирано.

Примери:
```haskell
> findBST 5 $ listToBST [5,4,6,3,7,2,8,1,9,0,10]
True

> findBST 69 $ listToBST [5,4,6,3,7,2,8,1,9,0,10]
False
```

## 02. Поточково преубразуване (`map`) на дървета
```haskell
mapTree :: (a -> b) -> Tree a -> Tree b
```

Имплементирайте `map` за дървета.

Примери:
```haskell
> mapTree succ $ Node 8 (Node 6 Empty Empty) (Node 7 Empty Empty)
Node 9 (Node 7 Empty Empty) (Node 8 Empty Empty)

> mapTree even $ Node 8 (Node 6 Empty Empty) (Node 7 Empty Empty)
Node True (Node True Empty Empty) (Node False Empty Empty)

> mapTree odd $ Node 8 (Node 6 Empty Empty) (Node 7 Empty Empty)
Node False (Node False Empty Empty) (Node True Empty Empty)
```

## 03. Сгъвания

Не забравяйте за уговорката за реда на обхождане!

### 03.00. Сгъване
```haskell
foldTree :: Monoid a => Tree a -> a
```

Искаме да конкатенираме всичките стойности в дърво, допускайки че те имат `Monoid`.

Примери:
```haskell
> foldTree Empty :: [Int]
[]

> foldTree $ Node [3,4] (Node [1,2] Empty Empty) (Node [5,6] Empty Empty)
[1,2,3,4,5,6]
```

### 03.01. The One Function
```haskell
foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
```

Като горното, но първо ще преубразуваме стойностите в дървото ни до такива, които имат
`Monoid`.

Примери:
```haskell
> foldMapTree id $ Node [3,4] (Node [1,2] Empty Empty) (Node [5,6] Empty Empty)
[1,2,3,4,5,6]

> getDual $ foldMapTree Dual $ Node [3,4] (Node [1,2] Empty Empty) (Node [5,6] Empty Empty)
-- ^ from Instances
[5,6,3,4,1,2]
```

### 03.02. Сумиране на дърво
```haskell
sumTree :: Num a => Tree a -> a
```

Използвайте [`Data.Monoid.Sum`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:Sum) и `foldTreeMap` за да имплементиранте сумиране на дърво.

Примери:
```haskell
> sumTree $ Node 8 (Node 6 Empty Empty) (Node 7 Empty Empty)
21
```

### 03.03. Проверка на "за всяко" за дърво
```haskell
allTree :: (a -> Bool) -> Tree a -> Bool
```

Използвайте [`Data.Monoid.All`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:All) и `foldTreeMap` за да проверите дали предикат
важи за всичките елементи на дърво.

Примери:
```haskell
> allTree even $ Node 8 (Node 6 Empty Empty) Empty
True

> allTree odd $ Node 7 (Node 6 Empty Empty) Empty
False
```

### 03.04. Правене на плоскост от дърво
```haskell
treeToList :: Tree a -> [a]
```

Използвайте подходящ моноид и `foldTreeMap` за да "спляскате" дърво в списък.

Примери:
```haskell
> treeToList $ Node 8 (Node 6 Empty Empty) Empty
[6,8]

> treeToList $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
[6,8,7,13]
```

### 03.05. Проверка за принадлежност
```haskell
elemTree :: Eq a => a -> Tree a -> Bool
```

Използвайте [`Data.Monoid.Any`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:Any) и `foldTreeMap` за да проверите дали дадена стойност
принадлежи на дърво.

Примери:
```haskell
> elemTree 8 $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
True

> elemTree 69 $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
False
```

### 03.05.00. `onMaybe`
```haskell
onMaybe :: (a -> Bool) -> a -> Maybe a
```
Полезно при следващите задачи.

Примери:
```haskell
> onMaybe even 5
Nothing

> onMaybe even 6
Just 6
```

### 03.06. Проверка за елемент, изпълняваш предикат
```haskell
findPred :: (a -> Bool) -> Tree a -> Maybe a
```

Използвайте `Instances.First/Last` (или пък `Data.Monoid.First/Last`) и `foldMapTree`, за
да проверите дали има елемент в дърво, изпълняващ даден предикат.

Примери:
```haskell
> findPred even $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 6

> findPred odd $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 7

> findPred (==0) $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Nothing
```

### 03.07. Всички елементи, изпълняващи предикат

Използвайте подходящ моноид и `foldMapTree` за да вземете всички стойности от дърво,
които изплъняват даден предикат.

Примери:
```haskell
> findAll even $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
[6,8]

> findAll odd $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
[7,13]

> findAll (==0) $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
[]
```

## 04. "Валидация" на дърво (обхождане със страничен ефект)

Ще имплементираме обхождане на дърво, с изчисление което може да има страничен ефект.

В този случай ще фиксираме страничният ни ефект да е "възможност за провал" - `Maybe`.

```haskell
traverseTreeMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
```

Искаме да обходим дърво с дадената функция, като ако в който и да е момент
получим `Nothing` ("изчислението ни е се провали"), искаме цялата сметка да спре.

Не забравяйте за уговорката за обхождане.

Примери:
```haskell
> traverseTreeMaybe (onMaybe even) $ Node 4 (Node 2 Empty Empty) (Node 0 Empty Empty)
Just (Node 4 (Node 2 Empty Empty) (Node 0 Empty Empty))

> traverseTreeMaybe (onMaybe even) $ Node 4 (Node 2 Empty Empty) (Node 0 (Node 3 Empty Empty) Empty)
Nothing

> traverseTreeMaybe (lookup 1) $ Node [(1, 'a')] (Node [(1, 'b')] Empty Empty) (Node [(1, 'c')] Empty
Empty)
Just (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty))

> traverseTreeMaybe (lookup 1) $ Node [(1, 'a')] (Node [(1, 'b')] Empty Empty) (Node [(2, 'c')] Empty
Empty)
Nothing

> traverseTreeMaybe (lookup 1) $ Node [(1, 'a')] (Node [(1, 'b')] Empty Empty) (Node [(2, 'l'), (1, 'c')] Empty
Empty)
Just (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty))
```

## 05. Пътища в дърво
```haskell
data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)
```

Чрез списък от този тип данни ще изразяваме път до даден елемент на дърво.

### 05.00. Стигане до елемент по път
```haskell
fetch :: [Direction] -> Tree a -> Maybe a
```

Извлечете елемент по даден път.

Примери:
```haskell
> fetch [] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 8

> fetch [L] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 6

> fetch [L,R] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Nothing

> fetch [R,R] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 13

> fetch [R,L] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Nothing

> fetch [R] $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
Just 7
```

### 05.01. Пътища до всички елементи
```haskell
paths :: Tree a -> [(a, [Direction])]
```
Имплементирайте функция, която по дадено дърво връща асоциативен списък от всеки
негов елемент и пътя до съотвения елемент.

**HINT/IDEA:** Може да ви е полезно да имплементирате функция която по дърво
връща "асоциативно дърво", закачайки на всеки елемент на оргиналното пътя до съответния елемент.
```haskell
mapDirections :: Tree a -> Tree (a, [Direction])
```

Примери:
```haskell
> paths $ Node 8 Empty Empty
[(8,[])]

> paths $ Node 8 Empty (Node 10 Empty Empty)
[(8,[]),(10,[R])]

> paths $ Node 8 (Node 13 Empty Empty) (Node 10 Empty Empty)
[(13,[L]),(8,[]),(10,[R])]

> paths $ Node 8 (Node 6 Empty Empty) (Node 7 Empty (Node 13 Empty Empty))
[(6,[L]),(8,[]),(7,[R]),(13,[R,R])]
```
