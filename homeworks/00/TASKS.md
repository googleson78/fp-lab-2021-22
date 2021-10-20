## Двоични числа

### `Bit`

Представяне на битове в Хаскел.

"Същото" като булеви стойности, но ще ги използваме за данни и двоични числа,
а не за control flow.

### `Binary`

Рекурсивна дефиниция на двоични числа.

С `End` ще означим край на двоичното ни число,
докато с конструктора (който е и оператор)
```haskell
(:.) :: Binary -> Bit -> Binary
```
можем да "разширяваме" отдясно двоично число.

Обърнете внимание на декларацията
```haskell
infixl 6 :.
```
Тя казва на Хаскел компилатора, че `(:.)` ще е **лявоасоциативен** оператор, т.е.
```haskell
End :. One :. Zero :. One
```
всъщност означава
```haskell
((End :. One) :. Zero) :. One
```
, а не
```haskell
End :. (One :. (Zero :. One))
```
, което е невалидна стойност, защото например в `Zero :. One` конструкторът би бил приложен над
два `Bit`-а, докато той взима за ляв/първи аргумент `Binary` стойност.

Това е важно също и когато pattern match-вате върху `(:.)` - получавате бит отдясно и остатъчното двоично число отляво.

Декларацията специфицира и приоритета на конструктора, но това вероятно не е съществено за домашното.



##### Примери за двоични числа представени чрез `Binary`

|Десетично|Двоично|`Binary`|
|-----|-----|-----|
|0|0|`End`|
|6|110|`End :. One :. One :. Zero`|
|5|101|`End :. One :. Zero :. One`|
|8|1000|`End :. One :. Zero :. Zero :. Zero`|
|42|101010|`End :. One :. Zero :. One :. Zero :. One :. Zero`|
|42|101010|`End :. Zero :. Zero :. Zero :. Zero :. Zero :. One :. Zero :. One :. Zero :. One :. Zero`|
|511|111111111|`End :. One :. One :. One :. One :. One :. One :. One :. One :. One`|

Обърнете внимание как всяко число има безкрайно много представяния - винаги можем да лепим водещи нули.

Например, в таблицата има две различни представяния на 42.

### Дефиниция - Канонично `Binary`
Канонично `Binary`, ще наричаме такова, което **няма** водещи нули, т.е. не изпълнява `hasLeadingZero` (вижте съответната задача).

### Бележка за тестовете

Има някои тестове, които са базирани на свойства, които дадени функции имат съвместно с други.

Например има тест за свойството "за всяко двоично число bin `binaryToInteger (succBinary bin) == 1 + binaryToInteger bin`".

Поради това е възможно не всички тестове да минават преди да са имплементирани всички функции (в този случай най-важните за тези тестове са `binaryToInteger` и `integerToBinary`).

Те са отделени в своя секция, която се казва `properties`.

## Задачи

### `succBinary :: Binary -> Binary`

Увеличете подаденото двоично число с 1.

##### Примери

```
> succBinary End
End :. One
> succBinary (End :. Zero)
End :. One
> succBinary (End :. One :. One :. One)
End :. One :. Zero :. Zero :. Zero
```

### `integerToBinary :: Integer -> Binary`

Конвертирайте даденото цяло число към двоично представяне.

Може да допуснете, че числото е неотрицателно.

Генерирайте канонични числа.

##### Примери

```haskell
> integerToBinary 0
End
> integerToBinary 69
End :. One :. Zero :. Zero :. Zero :. One :. Zero :. One
> integerToBinary 5
End :. One :. Zero :. One
> integerToBinary 7
End :. One :. One :. One
```

### `binaryToInteger :: Binary -> Integer`

Конвертирайте даденото двоично число към цяло.

##### Примери

```haskell
> binaryToInteger End
0
> binaryToInteger (End :. One :. Zero :. Zero :. Zero :. One :. Zero :. One)
69
> binaryToInteger (End :. One :. Zero :. Zero :. Zero :. Zero)
16
> binaryToInteger (End :. Zero :. One)
1
```

### `hasLeadingZero :: Binary -> Bool`

Проверете дали даденото число има водеща нула.

##### Примери

```haskell
> not $ hasLeadingZero End
True
> not $ hasLeadingZero $ End :. One
True
> hasLeadingZero $ End :. Zero
True
> hasLeadingZero $ End :. Zero :. One :. Zero
True
```

### `isEnd :: Binary -> Bool`

Направете функция която засича дали двоичното число е `End`.

Полезно за следващата задача.

### `canonicalise :: Binary -> Binary`

Конвертирайте даденото двоично число към канонично такова.

##### Примери

```haskell
> canonicalise End
End
> canonicalise (End :. One)
End :. One
> canonicalise (End :. Zero)
End
> canonicalise (End :. Zero :. One :. Zero)
End :. One :. Zero
```

### `addBinary :: Binary -> Binary -> Binary`

Имплементирайте събиране за двоични числа.

##### Примери

```haskell
> addBinary End (End :. One)
End :. One
> addBinary (End :. Zero) End
End :. Zero
> addBinary (End :. Zero) (End :. One)
End :. One
> addBinary (End :. One) (End :. Zero)
End :. One
> addBinary (End :. One :. Zero) (End :. One :. Zero :. Zero)
End :. One :. One :. Zero
```
