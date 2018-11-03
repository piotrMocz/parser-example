# Parser Example

Niniejsze repozytorium ma na celu przedstawienie paru rzeczy:
* testowania kodu w Haskellu (HSpec, QuickCheck)
* szeroko pojętych "dobrych praktyk" przy pisaniu kodu Haskella
* niektórych mniej lub bardziej zaawansowanych haskellowych pojęć

Warto zaznaczyć, że choć materiały w dużej części omawiają tworzenie parsera przy użyciu biblioteki [`megaparsec`](http://hackage.haskell.org/package/megaparsec), to jest on raczej pretekstem do przekazania powyższych informacji, niż celem samym w sobie (choć jest to bardzo dobra i nowoczesna biblioteka do pisania parserów).

## 1. Parsery

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


### Projekt w Haskellu

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
