module Parser where

import Data.Word
import Data.ByteString
import Data.Bits

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

word8 :: Parser Word8
word8 = Parser $ \bs -> do
    (h, t) <- uncons bs
    return (h, t)

fromOctets :: ByteString -> Word32
fromOctets = foldl' accum 0 where
    accum a b = (a `shiftL` 8) .|. fromIntegral b

word32 :: Parser Word32
word32 = Parser $ \bs -> do
    let octets = Data.ByteString.take 4 bs
        rest   = Data.ByteString.drop 4 bs
    return (fromOctets octets, rest)
