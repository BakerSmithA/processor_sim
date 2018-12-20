module Parser where

import Data.Word
import Data.Int
import Data.ByteString
import Data.Bits
import Control.Applicative
import Instr

newtype Parser a = Parser { runParser :: ByteString -> Maybe (a, ByteString) }

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Parser px) = Parser $ \bs -> do
        (x, bs') <- px bs
        return (f x, bs')

instance Applicative Parser where
    pure x = Parser $ \bs -> return (x, bs)

    -- f (a -> b) <*> f a -> f b
    Parser pf <*> Parser px = Parser $ \bs -> do
        (op, bs') <- pf bs
        (x, bs'') <- px bs'
        return (op x, bs'')

instance Monad Parser where
    return = pure

    -- m a >>= (a -> m b) -> m b
    Parser px >>= f = Parser $ \bs -> do
        (x, bs') <- px bs
        let Parser px' = f x
        px' bs'

instance Alternative Parser where
    empty = failure

    -- (<|>) :: m a -> m a -> m a
    Parser px <|> Parser py = Parser $ \bs ->
        case px bs of
            Nothing -> py bs
            Just r -> return r

failure :: Parser a
failure = Parser $ \_ -> Nothing

-- Parses byte from start of string.
word8 :: Parser Word8
word8 = Parser $ \bs -> do
    (h, t) <- uncons bs
    return (h, t)

fromOctets :: ByteString -> Word32
fromOctets = foldl' accum 0 where
    accum a b = (a `shiftL` 8) .|. fromIntegral b

-- Parses 4 bytes from string into Word32.
word32 :: Parser Word32
word32 = Parser $ \bs -> do
    if Data.ByteString.length bs < 4
        then Nothing
        else do
            let octets = Data.ByteString.take 4 bs
                rest   = Data.ByteString.drop 4 bs
            return (fromOctets octets, rest)

-- Parses 4 bytes from string into Int32.
int32 :: Parser Int32
int32 = fmap fromIntegral word32

-- Parses if the byte at the start of the string is the supplied byte, otherwise fails.
byte :: Word8 -> Parser Word8
byte w = do
    actual <- word8
    if w == actual
        then return w
        else failure

instr :: Parser FInstr
instr =
    -- Memory
        moveI        <$ byte 0  <*> word8 <*> int32
    <|> move         <$ byte 14 <*> word8 <*> word8
    <|> loadIdx      <$ byte 1  <*> word8 <*> word8 <*> int32
    <|> loadBaseIdx  <$ byte 2  <*> word8 <*> word8 <*> word8
    <|> storeIdx     <$ byte 3  <*> word8 <*> word8 <*> int32
    <|> storeBaseIdx <$ byte 4  <*> word8 <*> word8 <*> word8
    -- Arithmetic/Logic
    <|> add  <$ byte 5  <*> word8 <*> word8 <*> word8
    <|> addI <$ byte 16 <*> word8 <*> word8 <*> int32
    <|> sub  <$ byte 6  <*> word8 <*> word8 <*> word8
    <|> subI <$ byte 17 <*> word8 <*> word8 <*> int32
    <|> mult <$ byte 18 <*> word8 <*> word8 <*> word8
    <|> divI <$ byte 23 <*> word8 <*> word8 <*> word8
    <|> eq   <$ byte 7  <*> word8 <*> word8 <*> word8
    <|> lt   <$ byte 19 <*> word8 <*> word8 <*> word8
    <|> orI   <$ byte 8  <*> word8 <*> word8 <*> word8
    <|> andI <$ byte 9  <*> word8 <*> word8 <*> word8
    <|> notI <$ byte 15 <*> word8 <*> word8
    -- Branching
    <|> b       <$ byte 10 <*> word32
    <|> bt      <$ byte 11 <*> word8 <*> word32
    <|> bf      <$ byte 22 <*> word8 <*> word32
    <|> ret ()  <$ byte 12
    <|> sysCall <$ byte 21
    -- Debugging
    <|> printI  <$ byte 13 <*> word8
    <|> printC  <$ byte 24 <*> word8
    <|> printLn <$ byte 20

instrs :: Parser [FInstr]
instrs = many instr

parse :: Parser a -> ByteString -> Maybe a
parse (Parser p) bs = fmap fst (p bs)
