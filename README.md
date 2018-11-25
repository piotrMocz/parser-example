# Parser Example

Niniejsze repozytorium ma na celu przedstawienie paru rzeczy:
* testowania kodu w Haskellu (HSpec, QuickCheck)
* szeroko pojętych "dobrych praktyk" przy pisaniu kodu Haskella
* niektórych mniej lub bardziej zaawansowanych haskellowych pojęć

Warto zaznaczyć, że choć materiały w dużej części omawiają tworzenie parsera przy użyciu biblioteki [`megaparsec`](http://hackage.haskell.org/package/megaparsec), to jest on raczej pretekstem do przekazania powyższych informacji, niż celem samym w sobie (choć jest to bardzo dobra i nowoczesna biblioteka do pisania parserów).

## Parsery

Czym jest parser? Czymś, co przekształca tekst (w Haskellu może być to np. `String` albo `Text`) na jakiś ustrukturyzowany język formalny (najczęściej abstrakcyjne drzewo syntaktyczne jakiegoś języka). Przykładowo, możemy chcieć sparsować napis `"2 + 2 * 2"` do następującego obiektu: `Add (Lit 2) (Mul (Lit 2) (Lit 2))`. Dalej łatwo moglibyśmy napisać program, który wyliczy wartość takiego wyrażenia arytmetycznego.

```haskell
data Tree = Add Tree Tree
          | Mul Tree Tree
          | Lit Int

parse :: String -> Either String Tree
parse _ = undefined

eval :: Tree -> Int
eval (Lit n)     = n
eval (Add t1 t2) = eval t1 + eval t2
eval (Mul t1 t2) = eval t1 * eval t2
```

#### Ćwiczenie 1.1
Dopisz do powyższego programu implementację funkcji `parse`, używając w tym celu standardowych funkcji z modułu `base` i zwykłych operacji na napisach, typu `split`. Niepoprawne wejście powinno zwracać `Left <errorMessage>`. Jakie są zalety i wady takiego podejścia?

#### Ćwiczenie 1.2
Dopisz do powyższego programu również odejmowanie i dzielenie. Kto ustala priorytety operacji? Funkcja `parse`, czy funkcja `eval`?

Oczywiście, parserów możemy używać do dużo bardziej skomplikowanych zadań: parsowania języków programowania, plików w formatach takich, jak JSON czy YAML czy niestandardowych logów. My stworzymy swój własny język, oparty na Lispie, który będziemy sukcesywnie rozwijać.

### Megaparsec

Pisanie parserów od zera jest wykonalne, ale zbędne: istnieją do tego wspaniałe narzędzia. My wybierzemy [`megaparsec`](http://hackage.haskell.org/package/megaparsec), gdyż wydaje się obecnie być "state of the art" bibliotek do tworzenia parserów. Cały pomysł opiera się na pisaniu prostych parserów, "rozumiejących" najprostsze konstrukcje (pojedyncze znaki czy słowa) i składaniu ich w bardziej zaawansowane parsery.


## Projekt w Haskellu

Na dobry początek zaczniemy tworzyć nasz projekt.

```bash
$ stack new parser-example
```

W ten sposób dostaniemy nowy projekt `stack`, który będziemy mogli rozwijać. Żeby użyć pakietu `megaparsec`, musimy go dodać jako zależność projektu w `package.yaml`. W polu `dependencies` dodajemy `- megaparsec`. Teraz możemy wywołać `stack install` i pakiet powinien się ściągnąć i zainstalować. Stwórzmy również plik `src/Parsers.hs`, w którym będziemy tworzyć nasze parsery. Zaczniemy od deklaracji modułu i importów:

```haskell
module Parsers where

import           Text.Megaparsec      (Parsec)
import qualified Text.Megaparsec.Char as P
```

### Pierwszy parser

Nim przystąpimy do pisania faktycznych parserów, ułatwimy sobie życie: podstawowym typem w `megaparsecu` jest `Parsec`, który ma trzy parametry:
* typ błędów, zwracanych przez parser
* typ wejścia parsera
* typ zwracany przez parser

My, na potrzeby tego wykładu, będziemy pisać parser, który jako błędy zwraca napisy typu `String` (choć w produkcyjnym kodzie napisalibyśmy własne typy na błędy) i typu `String` jest jego wejście (choć w produkcyjnym kodzie lepiej używać typu `Text` z pakietu [`text`](http://hackage.haskell.org/package/megaparsec)). W takim razie możemy zdefiniować sobie dokładnie to:
```haskell
type Parser = Parsec String String
```

Zwróćmy uwagę, że trzeci parametr pozostawiamy "wolny", więc będziemy mogli stworzyć np. coś typu `Parser Foo`.

Zaczniemy od najprostszego możliwego przykładu: parsowania jednego znaku. Jako, że będziemy pisać język lispopodobny, będzie to znak `'('`. Do naszego pliku `src/Parsers.hs` dopiszmy funkcję:

```haskell
openingParen :: Parser Char
openingParen = P.char '('
```

Przetestujmy ją w GHCi (`stack ghci`):
```haskell
> import Text.Megaparsec (parse)
> parse openingParen "" "(hello)"
Right '('
> parse openingParen "" "hello)"
Left **BOOM**
```

Zignorujmy drugi parametr funkcji `parse`, który nie będzie nam potrzebny, i który możemy z powodzeniem ustawić jako pusty napis. Pierwszy parametr to parser, jakiego funkcja ma użyć, a trzeci to wejście, które chcemy sparsować. Widzimy zatem, że nasz prosty parser, zgodnie z tym, jak go napisaliśmy, czyta jeden znak z wejścia i zależnie od tego, czy jest tym, czego się spodziewa, zwraca albo `Right '('` (sukces) albo `Left <straszny błąd>`. `<straszny błąd>` będziemy docelowo chcieli przerobić na coś zrozumiałego dla ludzi, więc coś pokroju `"Couldn't parse the input: expected an opening paren"`. Całkiem rozsądnym pytaniem będzie: co dzieje się z resztą wejścia? O tym w późniejszych rozdziałach.

#### Trochę zamieszania dla ułatwienia

(uwaga: to, co tutaj pokażemy, jest niby-proste, a jednak warto wiedzieć, jak sobie radzić ze skomplikowanymi sygnaturami)

Jak widzimy, funkcja `parse` ma parametr, którego nie będziemy używać. Dodatkowo, jej typ jest nieco zbyt skomplikowany, jak dla naszych potrzeb. Zróbmy więc prostą rzecz: napiszmy własną funkcję `parse`, która będzie miała dobry typ i jeden argument mniej.

Musimy zacząć od importu:
```haskell
import qualified Text.Megaparsec as P
```
Importujemy w ten sposób, bo gdy nazwiemy naszą funkcję `parse`, nie będziemy mieć konfliktów nazw.

Następnie zdefiniujmy sobie naszą funkcję:
```haskell
parse parser input = P.parse parser "" input
```
(uwaga: parametr input możnaby po obu stronach usunąć, przerabiając funkcję na wariant bezpunktowy, ale w zasadzie w ten sposób jest czytelniej).

Jaki ma typ? W GHCi:
```haskell
> :t parse
parse :: Text.Megaparsec.Parsec e s a -> s -> Either(Text.Megaparsec.Error.ParseErrorBundle s e) a
```
Hmm, zbyt skomplikowane. Ale przecież mamy `type Parser = Parsec String String`! Czyli możemy zacząć pisać sygnaturę:
```haskell
parse :: Parser a -> String -> Either _ a
parse parser input = P.parse parser "" input
```
Nie jesteśmy jeszcze pewni, co będzie tym dziwnym pierwszym parametrem `Either`. Nie szkodzi, kompilator nam pomoże. Jeśli zastąpimy go `_` i spróbujemy skompilować kod, dostaniemy:
```
* Found type wildcard `_'
    standing for `P.ParseErrorBundle String String'
```
Świetnie! Zatem możemy sobie dla ułatwienia zdefiniować:
```haskell
type ParserError = P.ParseErrorBundle String String
```
i uzupełnić naszą funkcję:
```haskell
parse :: Parser a -> String -> Either ParserError a
parse parser input = P.parse parser "" input
```
Całkiem ładna sygnatura! Pozbywamy się polimorfizmu błędów i strumienia wejściowego, ale zyskujemy dużo większą czytelność kodu. Dla mnie -- ekstra.

### Testy pierwszego parsera

Jak każdy szanujący się programista, napiszemy do naszego programu testy jednostkowe. Zaczniemy od dodania do zależności w `package.yaml` pakietu `hspec` (standardowy framework do testów jednostkowych w Haskellu). Wystarczy, że dodamy go w polu `dependencies` w sekcji `test` -- reszta aplikacji nie potrzebuje modułu do testów, więc często nie będzie potrzeby ich budować.

Skoro mamy już taki pakiet, możemy zacząć pisanie testów. W pliku `test/Spec.hs` zaimportujemy sobie dwie rzeczy:
1. narzędzia do testów:
```haskell
import Test.Hspec
```
2. moduł, który będziemy testować:
```haskell
import Parsers
```

(uwaga: ogólnie rzecz biorąc chcemy używać wyłącznie importów kwalifikowanych (`import qualified M as N`) albo importować konkretne symbole `import M (a, b)`), ale w testach do naszego prostego przykładu możemy na chwilę sobie pozwolić na odrobinę niedbałości).

Testy piszemy w formie: `<ten obiekt> powinien robić <to>`. Hspec ma do tego ładne funkcje, zapiszmy więc w jego języku "parser `openingParen` powinien parsować napis `"("` i zwracać `Right '('`". W zasadzie sprowadza się to do przetłumaczenia tego na angielski:
```haskell
describe "The open paren parser" $
    it "should parse the \"(\" string" $
        parse openingParen "(" `shouldBe` Right '('
```
Całkiem czytelnie. Oczywiście "The open [...]" oraz "should parse [...]" to tylko komentarze dla programisty, pomocne przy wyświetlaniu wyników testu -- tylko ostatnia linijka mówi Hspecowi, co faktycznie ma przetestować. Tak czy owak, bardzo intuicyjne podejście, bardzo podobne do RSpeca (zresztą: zbieżność nazw jest nieprzypadkowa).

Musimy to jeszcze tylko ubrać w funkcję i wywołać:
```haskell
openingParenSpec :: Spec
openingParenSpec = describe "The open paren parser" $ do
    it "should parse the \"(\" string" $
        parse openingParen "(" `shouldBe` Right '('

main :: IO ()
main = hspec $ openingParenSpec
```
Co robi funkcja `hspec`? Popatrzmy na jej typ: `hspec :: Spec -> IO ()`. Po prostu wykonuje testy.

Możemy teraz w konsoli uruchomić nasz test:
```bash
$ stack test
```
(Wyjście pomijamy, ale powinniśmy zobaczyć zielony napis, informujący o powodzeniu testu).

Możemy dopisywać następne testy do tego samego parsera dodając kolejne bloki `it "[...]"` w środku funkcji `describe` (kod poniżej). Co jednak ze sprawdzeniem, czy parser zgłąsza błąd wtedy, kiedy trzeba? Błędy są na razie dość zawiłe, więc nie będziemy sprawdzać, czy parser zwrócił konkretny błąd -- wystarczy nam upewnić się, że dla złego wejścia zwrócił `Left`. Do tego celu przyda nam się funkcja `isLeft :: Either l r -> Bool`, która co prawda znajduje się w pakiecie `extra`, ale czytelnik dopisze ją sam, jako ćwiczenie.

#### Ćwiczenie 1.3

Napisz funkcję `isLeft` opisaną wyżej. `isLeft $ Left undefined` powinno zwrócić `True`, `isLeft $ Right $ error "an error"` powinno zwrócić `False`. Uwaga: przetestuj funkcję również na tych konkretnie przykładach.

Mamy zatem:
```haskell
openingParenSpec :: Spec
openingParenSpec = describe "The open paren parser" $ do
    it "should parse the \"(\" string" $
        parse openingParen "(" `shouldBe` Right '('

    it "should fail on the \"hello\" string" $
        isLeft (parse openingParen "hello") `shouldBe` True
```

To już coś! (Zwróćmy uwagę na dodane `do`, które umożliwia nam użycie wielu `it`).

#### Ćwiczenie 1.4

Dopisz jeszcze po jednym przykładzie testowym na poprawne i niepoprawne parsowanie.

### Powtarzalność

Czyżbyśmy robili przysłowiową "robotę głupiego"? Wydaje się, że przykłady testowe, które piszemy dla różnych wejść, są wszystkie strasznie podobne. Czy nie ma jakiejś biblioteki, która zrobiłaby to za nas? Poszukała wielu przypadków (w tym brzegowych) tak, żeby zweryfikować nie tyle konkretne przypadki, co pewne właściwości?

Tak! Istnieje, i nazywa się QuickCheck!

Dodamy do naszego `package.yaml` dodamy zależność `quickcheck` (najlepiej w sekcji `test` -- tak, aby wszystkie frameworki do testów nie musiały się dołączać do "produkcyjnego" kodu, zwiększając czas budowania) i do pliku `Spec.hs` linijkę:
```haskell
import Test.QuickCheck
```

Choć QuickCheck w żaden sposób nie zależy od HSpeca, dobrze się z nim integruje, dostarczając ładnej składni, którą już znamy. Spróbujmy uogólnić testy, które pisaliśmy poprzednio. Chcielibyśmy sprawdzić, czy "parser `openingParen` zwraca `Right '('` dla każdego napisu zaczynającego się od otwierającego nawiasu. Popatrzmy:
```haskell
openingParenPropSpec :: Spec
openingParenPropSpec = describe "A single character parser" $ do
    it "should accept this character" $ property $
        \s -> parse openingParen ('(':s) `shouldBe` Right '('
```

Wciąż wygląda to prawie jak język angielski. Różnica jest taka, że teraz, zamiast podawać konkretny przypadek, który musi być prawdziwy, podajemy pewną właściwość. Za pomocą funkcji `property` mówimy: "dla dowolnego (napisu) `s` musi zachodzić [...]". QuickCheck automagicznie potrafi sprawdzić wiele różnych wartości `s`, dobrze sobie radząc z szukaniem przypadków brzegowych.

### Ćwiczenie 1.5

Dopisz do `openingParenPropSpec` test na napisy, które powodują, że parser zwraca `Left`. Jak upewnić się, że napis będzie dowolny, **ale** nie będzie się zaczynał od `(`?

### Ćwiczenie 1.6

Napisz test, który zachodzi dla **prawie** wszystkich przypadków i sprawdź, czy QuickCheck go znajdzie.

### Pytanie

Jak to się dzieje, że możemy dodać `property` i lambdę zamiast po prostu `x shouldBe y`? Sprawdź typy funkcji `it` i `property`. Pomocne mogą się okazać, w GHCi, komendy `:t` i `:i`.

## Rozwijanie parsera

Potrafimy parsować jeden znak -- przydałoby się jednak umieć coś więcej. Parsery możemy składać, tak, by z prostszych kawałków złożyć coś, co potrafi parsować bardziej skomplikowane rzeczy. Zacznijmy dość prosto: parser, który akceptuje nawias otwierający **lub** zamykający:
```haskell
paren :: Parser Char
paren = openingParen <|> closingParen

-- dodaliśmy oczywiście jeszcze tą funkcję:
closingParen :: Paren Char
closingParen = P.char ')'
```

Możemy przetestować, czy wszystko działa:
```haskell
> parse paren "(hello)"
Right '('
> parse paren ")hello"
Right ')'
> parse paren "hello"
Left <okropny błąd>
```
Wydaje się działać. Oczywiście, nie poprzestaniemy na wydawaniu się: dopiszemy testy.

### Ćwiczenie 2.1

Napisz w pliku `test/Spec.hs` dopisz:
1. Testy jednostkowe, sprawdzające, czy parser `paren` działa dla otwierających i zamykających nawiasów.
2. Testy jednostkowe, sprawdzające, czy parser `paren` poprawnie zwraca rezultat `Left <...>` dla niepoprawnych napisów.
3. Testy QuickChecka, sprawdzające, czy dowolny napis zaczynający się od `(` lub `)` jest akceptowany przez parser.

### Prawdziwe oblicze alternatywy

Zastanówmy się przez chwilę, czym jest magiczne `<|>`, którego używamy? Czy to jakaś magiczny operator Megaparseca? Nie do końca:
```haskell
> :t (<|>)
(<|>) :: GHC.Base.Alternative f => f a -> f a -> f a
```

`Alternative` to typeklasa, która jest abstrakcją stworzoną do właśnie takich celów.
```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```
Zobaczmy, jak działa to dla `Maybe`:
```haskell
> Nothing | Nothing
Nothing
> Just 42 | Nothing
Just 42
> Nothing | Just 42
Just 42
> Just 42 | Just 43
Just 42
```
Widać zachowanie bardzo podobne, jak w przypadku naszych parserów. Swoją drogą, dla list `<|>` to po prostu konkatenacja.

## Rozwijanie, ciąg dalszy

Na razie jesteśmy w stanie parsować pojedyncze znaki, co jest imponujące, ale nie jesteśmy w stanie wiele zrobić. Jak zatem połączyć parsery w dłuższe sekwencje?

Spróbujmy na początek trywialnego przykładu: sparsujemy otwierający nawias, a następnie nawias zamykający. Nie jest to samo w sobie zbyt użyteczne, ale nauczymy się przydatnych rzeczy. Przede wszystkim: Megaparsec modeluje rzeczy następujące po sobie w sekwencji tak, jak większość Haskellowego światka: za pomocą monad. Instancje dla parserów napisane są tak, by po kolei parsowały i "zjadały" kawałki wejściowego strumienia. Jeśli któryś z parserów nie zaakceptuje wejścia, cała sekwencja zwróci błąd.

Najpierw przyglądniemy się, jak to działa w praktyce, a dopiero później zaglądniemy "pod maskę", by poznać nieco lepiej ideę działania parserów i ich składania.
```haskell
emptyParens :: Parser Char
emptyParens = openingParen >> closingParen
```
Możemy sprawdzić w GHCi, czy działa tak, jakbyśmy się tego spodziewali:
```haskell
> parse emptyParens "()"
Right ')'
```
Zwróćmy uwagę, że każdy nasz parser ma typ `Parser Char`, więc zawsze będzie zwracał tylko jeden znak. Docelowo chcemy, żeby parser zwrócił coś bardziej ustrukturyzowanego (AST) -- zajmiemy się tym w dalszych odcinkach.

### Ćwiczenie 2.2

Napisz testy (HSpec + QuickCheck) testujące parser `emptyParens`.

### Parser pod maską

Powiedzieliśmy, że przyglądniemy się, jak parser działa "pod maską". Zamiast oglądać implementację Megaparseca (która jest mocno zoptymalizowana -- w większości języków obniża to czytelność kodu, a co dopiero w Haskellu), spróbujmy napisać własny, prosty parser. (Pożyczymy implementację z [tej odpowiedzi na Stack Overflow](https://stackoverflow.com/questions/20660782/writing-a-parser-from-scratch-in-haskell), ale nasz parser będzie miał jeszcze instancję monady).

Zacznijmy od definicji typu `Parser`:
```
type Error = String
newtype Parser a = P { unP :: String -> (String, Either Error a) }
```

Parser to po prostu `newtype` nad funkcją, która przyjmuje napis wejściowy i zwraca parę: pierwszy element to pozostały fragment napisu wejściowego. Drugi to rezultat parsowania, który może być albo `Right <to, co zwraca parser>`, albo `Left <błąd parsowania>`. Łatwo zauważyć, że taka definicja sama w sobie nie pozwoli nam na zbyt wielką swobodę składania parserów (w końcu: moglibyśmy napisać parser jako jedną funkcję z wielką plątaniną `if`-ów, ale po to programujemy funkcyjnie, żeby rzeczy dobrze się "komponowały"). Powyżej widzieliśmy, że instancje `Alternative` i `Monad` dają naszym parserom duże możliwości -- napiszmy je więc. (W dzisiejszych czasach każda monada musi być również aplikatywem, więc musimy napisać instancje: `Functor`, `Applicative` i `Monad`).

Najpierw instancja `Functor`:
```haskell
instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (rest, Left err) -> (rest, Left err)
    (rest, Right a ) -> (rest, Right (f a))
```
Dość proste: bierzemy nasz stary parser, wyciągamy z niego funkcję parsującą, a następnie tworzymy nową funkcję: taką, która zaaplikuje starą funkcję na swoim parametrze. Jeśli rezultat to `Right`, to zaaplikujemy `f` w środku. Jeśli rezultatem był błąd `Left`, zostawimy go jak jest. Warto zwrócić uwagę, że `st stream` nie wykona się od razu -- dopiero, jak ktoś "odpakuje" obiekt typu `Parser`. To jeden z uroków leniwej ewaluacji: w większości przypadków kopie nas w kostkę i spowalnia programy, ale czasami pozwala na eleganckie abstrakcje.

Następnie instancja `Applicative`:
```haskell
instance Applicative Parser where
  pure a = P (\stream -> (stream, Right a))
  P f1 <*> P xx = P $ \stream0 -> case f1 stream0 of
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))
```
Funkcja `pure` jest prosta: zwraca swój argument "opakowany" w całą maszynerię parsera. Funkcja `<*>` (na którą ponoć mówi się "ap") robi podobną rzecz, którą robiło `fmap`, tylko dwa razy. 

### Ćwiczenie 2.3

Mając instancję `Applicative`, nie jest tak trudno wymyślić instancję monady (swoją drogą -- istnieją też parsery aplikatywne, ale my chcemy naśladować Megaparseca). Napisz instancję `Monad` dla naszego parsera (tak naprawdę wystarczy napisać metodę `>>=`).

#### Wskazówka

Sygnatury są następujące:
```haskell
instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = ???
```

### Parsowanie pojedynczych znaków

Mamy monady, więc moglibyśmy już parsować sekwencje znaków. Ale nie umiemy parsować pojedynczych znaków! Faktycznie -- nieco się zapędziliśmy. Nasz parser wymaga jeszcze nieco "pracy u podstaw". Zdefiniujmy sobie funkcję, która pozwoli podglądnąć znak i zaakceptować go, jeśli spełnia podany w argumencie predykat. Oto ona:
```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \stream -> case stream of
    []                 -> ([], Left "end of stream")
    (c:cs) | f c       -> (cs, Right c)
           | otherwise -> (cs, Left "did not satisfy")
```

### Ćwiczenie 2.4

Używając funkcji `satisfy`, napisz funkcję `char`, parsującą konkretny znak. Jej sygnatura to: `char :: Char -> Parser Char`.

### Alternatywy (4?)

Megaparsec potrafił parsować `to` **lub** `tamto`. Wypada nam dopisać to do naszego zabawkowego parsera, żeby zrozumieć, jak to może działać. Interesuje nas funkcja, która dostanie dwa parsery i wykona następujące kroki:
1. Uruchomi pierwszy parser na danym wejściu. Jeśli je zaakceptował, powinna zwrócić dobry rezultat.
2. Jeśli pierwszy parser odrzucił wejście, zignoruje jego rezultat i uruchomi drugi parser. Wtedy już nie ma wyboru: musi po prostu zwrócić jego rezultat.
Oczywiście funkcja nie tyle te kroki wykona, co zwróci funkcję, która je wykona, gdy zostanie uruchomiona. A to wszystko jeszcze opakowane w parserowy newtype!

Kod może wyglądać tak:
```haskell
orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
    (stream1, Left _)  -> f2 stream1
    (stream1, Right a) -> (stream1, Right a)
```

To mały krok dla ludzkości, ale wielki dla naszego małego parsera: teraz możemy zdefiniować dla niego instancję `Alternative`. No więc proszę:
```haskell
instance Alternative Parser where
    empty = P $ \stream -> (stream, Left "empty")
    (<|>) = orElse
```

Nasza biblioteka robi się już całkiem użyteczna. Gdybyśmy jednak nie czuli potrzeby nieustannego doskonalenia naszego kodu, zapewne pisalibyśmy w języku innym, niż Haskell. Warto zatem nadmienić, że klasa `Alternative` ma jeszcze metody `some` oraz `many`. Mają defaultowe implementacje, ale możemy je przesłonić wydajniejszymi (podobnie jest na przykład z klasą `Monad`, która ma defaultową implementację `>>`, ale można napisać własną, jeśli z jakichś powodów mamy na nią dobry pomysł). Zróbmy to jako ćwiczenie:

### Ćwiczenie 2.5

Napisz metody:
```haskell
many :: Parser a -> Parser [a]
some :: Parser a -> Parser [a]
```
które dostaną parser jako argument i będą akceptować odpowiednio: 0 lub więcej oraz 1 i więcej powtórzeń wejścia, które akceptuje ich parser argument. Przykładowo: `some $ char 'a'` zaakceptuje napisy `"ab"` i `"aaaabb"`, a odrzuci `"bb"`. `many $ char 'a'` zaakceptuje wszystkie z nich.

### Jazda próbna parsera

Do pełni szczęścia brakuje nam tylko drobnej funkcji owijającej wywołania parsera (dla wygody):
```haskell
parse :: Parser a -> String -> Either Error a
parse parser input = snd $ (unP parser) input
```

Teraz możemy uruchomić `stack ghci` i sprawdzić, czy wszystko parsuje się zgodnie z naszymi oczekiwaniami:
```haskell
> let emptyParens = char '(' >> char ')'
> parse emptyParens "()"
Right ')'
> parse emptyParens ")"
Left "did not satisfy"
> parse emptyParens "("
Left "end of stream"
```
Podobnie możemy sprawdzić metody `some`, `many` oraz `<|>`.

### Ćwiczenie 2.6

Dopisz w HSpecu i QuickChecku testy naszego zabawkowego parsera. Możemy go dla wygody umieścić w pliku `src/MockParser.hs`. Należy jednak pamiętać, by w testach importować go w sposób kwalifikowany (np. `import qualified MockParser as MP`) -- w przeciwnym razie nazwy funkcji będą konfliktowały z Megaparsekiem.

### Uwagi końcowe i przemyślenia

Rozumiemy już, jaki pomysł stoi za takim podejściem do parsowania i widzimy, jak sprytnie napisane instancje `Monad` oraz `Alternative` pozwalają nam na składanie prostych parserów w bardziej skomplikowane. Ale chwileczkę... Czy na pewno jest konieczne, żeby nasz parser był monadą? Czyżbyśmy wykonali pracę na darmo? Przecież istnieją aplikatywne parsery! W tym celu zróbmy dwie rzeczy: jedno ćwiczenie (ćwiczonko) i jedno przemyślenie.

### Ćwiczenie 2.7

Jedyną rzeczą z monady, z jakiej korzystaliśmy, jest operator `>>`. Pozwala nam na parsowanie jednej rzeczy po drugiej, w sekwencji. Da się ten sam efekt osiągnąć tylko przy pomocy instancji `Applicative` naszego parsera. Napisz funkcję `andThen :: Parser a -> Parser b -> Parser b`, która wykona dwa parsery w sekwencji tak, jak widzieliśmy powyżej.

### Wskazówka

Pomocna może się okazać funkcja `seq`, która skądinąd jest dość ciekawa.

### Przemyślenie

Co "potrafi" parser monadyczny, czego nie potrafiłby parser aplikatywny?

### Wkazówka

Patrząc na ćwiczenie powyżej: czym się różni (w kontekście naszego parsera) `>>` od `>>=`? Co ignoruje `>>`?

Być może ciekawsza dyskusja to ewentualna przewaga parsera aplikatywnego nad monadycznym: wykracza to nieco poza zakres tego kursu, więc zainteresowanych pozostaje mi odesłać do internetu, który jest pełen opracowań na ten temat. Nam wystarczy wiedza, że Megaparsec jest parserem monadycznym, więc z takim będziemy mieć przez resztę kursu do czynienia -- dla naszych potrzeb będzie to na pewno dobra decyzja.
