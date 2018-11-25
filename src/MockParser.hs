module MockParser where

import Control.Applicative


type Error = String
newtype Parser a = P { unP :: String -> (String, Either Error a) }


instance Functor Parser where
    fmap f (P st) = P $ \stream -> case st stream of
      (rest, Left err) -> (rest, Left err)
      (rest, Right a ) -> (rest, Right $ f a)

instance Applicative Parser where
    pure a = P $ \stream -> (stream, Right a)
    P f1 <*> P xx = P $ \stream0 -> case f1 stream0 of
        (stream1, Left err) -> (stream1, Left err)
        (stream1, Right f)  -> case xx stream1 of
            (stream2, Left err) -> (stream2, Left err)
            (stream2, Right x)  -> (stream2, Right $ f x)

instance Monad Parser where
    return = pure
    P f1 >>= mf = P $ \stream -> case f1 stream of
        (stream1, Left  err) -> (stream1, Left err)
        (stream1, Right res) ->
            let f2 = unP (mf res) in f2 stream1

instance Alternative Parser where
    empty = P $ \stream -> (stream, Left "empty")
    (<|>) = orElse

    many = manyParser
    some = someParser


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \stream -> case stream of
    []                 -> ([], Left "end of stream")
    (c:cs) | f c       -> (cs, Right c)
           | otherwise -> (cs, Left "did not satisfy")

orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
    (stream1, Left _)  -> f2 stream1
    (stream1, Right a) -> (stream1, Right a)

manyParser :: Parser a -> Parser [a]
manyParser (P f) = P go where
    go stream = case f stream of
        (_,       Left _)  -> (stream, Right [])
        (stream', Right a) -> case go stream' of 
            (streamFin, Left err) -> (streamFin, Left err)
            (streamFin, Right as) -> (streamFin, Right (a : as))

someParser :: Parser a -> Parser [a]
someParser (P f) = P $ \stream -> case f stream of
    (stream', Left err) -> (stream', Left err)
    (stream', Right a)  -> 
        let (P fmany) = manyParser (P f)
        in case fmany stream' of
            (stream'', Left err) -> (stream'', Left err)
            (stream'', Right as) -> (stream'', Right (a:as))

char :: Char -> Parser Char
char c = satisfy (== c)

parse :: Parser a -> String -> Either Error a
parse parser input = snd $ (unP parser) input