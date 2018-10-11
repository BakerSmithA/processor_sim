module Parser where

import Data.Word
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
failure = Parser $ \bs -> Nothing

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

-- Parses if the byte at the start of the string is the supplied byte, otherwise fails.
byte :: Word8 -> Parser Word8
byte w = do
    actual <- word8
    if w == actual
        then return w
        else failure

moveI :: Parser Instr
moveI = MoveI <$ byte 0 <*> word32 <*> word32

instr :: Parser Instr
instr =
    -- Memory
        MoveI        <$ byte 0 <*> word32 <*> word32
    <|> LoadIdx      <$ byte 1 <*> word32 <*> word32 <*> word32
    <|> LoadBaseIdx  <$ byte 2 <*> word32 <*> word32 <*> word32
    <|> StoreIdx     <$ byte 3 <*> word32 <*> word32 <*> word32
    <|> StoreBaseIdx <$ byte 4 <*> word32 <*> word32 <*> word32
    -- Arithmetic/Logic
    <|> Add  <$ byte 5 <*> word32 <*> word32 <*> word32
    <|> AddI <$ byte 6 <*> word32 <*> word32 <*> word32
    <|> Sub  <$ byte 7 <*> word32 <*> word32 <*> word32
    <|> SubI <$ byte 8 <*> word32 <*> word32 <*> word32
    -- Branching
    <|> B   <$ byte 9  <*> word32
    <|> BGT <$ byte 10 <*> word32 <*> word32
    <|> Ret <$ byte 11

instrs :: Parser [Instr]
instrs = many instr

parse :: Parser a -> ByteString -> Maybe a
parse (Parser p) bs = fmap fst (p bs)
