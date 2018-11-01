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
