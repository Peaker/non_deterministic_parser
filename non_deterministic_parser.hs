{-# OPTIONS -O2 -Wall #-}
import Control.Monad.Trans(lift)
import Control.Monad(join)
import Control.Applicative((<$>), liftA2)
import Data.Maybe(fromMaybe)
import Data.List(nub)
import Data.Either(rights)
import Text.Parsec(ParsecT, runParserT, ParseError
                  ,oneOf, many1, digit, optionMaybe)

type Parser u m a = ParsecT String u m a

type Op = Char
data Tree = Leaf Int | Branch Tree Op Tree
  deriving (Eq, Show)

ops :: Monad m => Parser u m Char
ops = oneOf "*+"

leaf :: Monad m => Parser u m Tree
leaf = Leaf . read <$> many1 digit

lassocparserextend :: Monad m =>
                     (Tree -> Parser u m Tree)
                   -> Tree -> Parser u m Tree
lassocparserextend recurse left = do
  extension <- optionMaybe $ liftA2 (Branch left) ops leaf
  case extension of
    Nothing -> return left
    Just ext -> recurse ext

rassocparserextend :: Monad m => Parser u m Tree
                   -> Tree -> Parser u m Tree
rassocparserextend recurse left =
  fmap (fromMaybe left) . optionMaybe $
       liftA2 (Branch left) ops recurse

anyassocparser :: Parser u [] Tree
anyassocparser = do
  left <- leaf
  extends left
  where
    extends left =
      join $ lift $ [
                    lassocparserextend extends left,
                    rassocparserextend anyassocparser left
                   ]

parseIt :: Monad m => Parser () m a -> String -> m (Either ParseError a)
parseIt parser = runParserT parser () "<input>"

main :: IO ()
main = mapM_ print $ nub $ rights $
       [
       --  runIdentity $ parseIt lassocparser "1+2+3"
       -- ,runIdentity $ parseIt rassocparser "1+2+3"
       ] ++ parseIt anyassocparser "1+2+3"


-- Optionally:

-- import Data.Function(fix)

-- lassocparser :: Monad m => Parser u m Tree
-- lassocparser = leaf >>= fix lassocparserextend

-- rassocparser :: Monad m => Parser u m Tree
-- rassocparser = do
--   left <- leaf
--   rassocparserextend rassocparser left
