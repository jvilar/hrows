{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Col (
    -- *Types
    Col(..)
    -- *Functions
    , parseCols
    , applyCols
) where

import Control.Lens (Traversal', (%~), traversed, (&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Model.Row
import Model.RowStore
import Model.Expression.Evaluation
import Model.Expression.Manipulate
import Model.Expression.Lexer (Token(EOFT, CommaT, ColonT, OpenSBT, CloseSBT))
import Model.Expression.Parser
import Model.Expression.RecursionSchemas


-- |A Col especifies a column of the input in the command line. Single
-- expressions especify a column, a couple of expressions that correspond
-- each to a column, especify a range. And `AllCols` espcifies all the cols
-- in the input.
data Col = Single Expression | Range Expression Expression | AllCols deriving Show

expressionT :: Traversal' Col Expression
expressionT f (Single e) = Single <$> f e
expressionT f (Range e1 e2) = Range <$> f e1 <*> f e2
expressionT _ AllCols = pure AllCols

-- |Parse a list of expressions separated by commas, return
-- the corresponding list of `Col` or an error message
parseCols :: Text -> Either Text [Col]
parseCols = parse colParser

colParser :: Parser [Col]
colParser = do
    t <- current
    c <- case t of
             OpenSBT -> rangeParser
             _ -> Single <$> expression
    check EOFT >>= \case
        True -> return [c]
        False -> expect CommaT "a comma" >> (c:) <$> colParser

rangeParser :: Parser Col
rangeParser = do
    advance
    e1 <- expression
    checkPosition e1
    expect ColonT "a colon"
    e2 <- expression
    checkPosition e2
    expect CloseSBT "a closing square bracket"
    return $ Range e1 e2

checkPosition :: Expression -> Parser ()
checkPosition (In (Position _)) = return ()
checkPosition (In (NamedPosition _ _)) = return ()
checkPosition e = parsingError $ T.concat [ "Expression "
                                          , toFormula e
                                          , " does not represent a position"
                                          ]

-- |Produce a `RowStore` from a list of `Col` and an existing
-- `RowStore`.
applyCols :: [Col] -> RowStore -> RowStore
applyCols cs0 rst = case names rst of
                      Nothing -> fromRows "cols" . map (processRow cs) $ rows rst
                      Just _ -> fromRowsNames "cols" ns . map (processRow cs) $ rows rst
    where cs = cs0 & traversed . expressionT %~ addPositions rst
          ns = concatMap toName cs
          toName (Single e) = [toFormula e]
          toName (Range e1 e2) = case names rst of
                   Nothing -> [ T.pack ("Column " ++ show i) | i <- [p1 + 1..p2 + 1] ]
                   Just xs -> slice p1 p2 xs
               where p1 = pos e1
                     p2 = pos e2
          toName AllCols = fromMaybe [] (names rst)

processRow :: [Col] -> Row -> Row
processRow cs r = concatMap f cs
    where f (Single e) = [evaluate r [] e]
          f (Range e1 e2) = slice (pos e1) (pos e2) r
          f AllCols = r

slice :: Int -> Int -> [a] -> [a]
slice p1 p2 = take (p2 - p1 + 1) . drop p1

pos :: Expression -> Int
pos (In (Position n)) = n
pos (In (NamedPosition _ (Just n))) = n
pos e = error $ "Expression "
                ++ T.unpack (toFormula e)
                ++ " does not represent a position"
