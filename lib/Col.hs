{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Col (
    -- *Types
    Col(..)
    -- *Functions
    , parseCols
    , applyCols
    , slice
    , pos
    -- *Lenses
    , expressionT
    , colF
) where

import Control.Lens (Traversal', (%~), traversed, (&), Fold, folding)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Model.Row
import Model.RowStore
import Model.Expression.Evaluation
import Model.Expression.Manipulate
import Model.Expression.Lexer (Token(EOFT, CommaT, ColonT, OpenSBT, CloseSBT, StringT))
import Model.Expression.Parser
import Model.Expression.RecursionSchemas


-- |A Col especifies a column of the input in the command line. Single
-- expressions especify a column, a couple of expressions that correspond
-- each to a column, especify a range. And `AllCols` espcifies all the cols
-- in the input.
data Col = Single Expression (Maybe Text)
         | Range Expression Expression
         | AllCols deriving Show

-- |A traversal of the expressions in the `Col`
expressionT :: Traversal' Col Expression
expressionT f (Single e mn) = Single <$> f e <*> pure mn
expressionT f (Range e1 e2) = Range <$> f e1 <*> f e2
expressionT _ AllCols = pure AllCols

-- |A fold of the Fields specified by a `Col`
colF :: Col -> Fold RowStore Row
colF col = folding (map (processRow [col]) . rows)

-- |Parse a list of expressions separated by commas, return
-- the corresponding list of `Col` or an error message
parseCols :: Text -> Either Text [Col]
parseCols = parse colParser

colParser :: Parser [Col]
colParser = do
    c <- current >>= \case
             OpenSBT -> rangeParser
             _ -> do
                    e <- expression
                    check OpenSBT >>= \case
                        False -> return $ Single e Nothing
                        True -> do
                            advance
                            current >>= \case
                                StringT s -> do
                                    advance
                                    expect CloseSBT "a closing square bracket"
                                    return $ Single e (Just $ T.pack s)
                                t -> parsingError $ T.concat ["Expected a name for the column, found a "
                                                             , T.pack $ show t]

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
                      Nothing -> fromRows rn . map (processRow cs) $ rows rst
                      Just _ -> fromRowsNames rn ns . map (processRow cs) $ rows rst
    where rn = getName rst
          cs = cs0 & traversed . expressionT %~ addPositions rst
          ns = concatMap toName cs0
          toName (Single (In (NamedPosition n _)) Nothing) = [n]
          toName (Single (In (Position p)) Nothing) = [fnames rst !! p]
          toName (Single e Nothing) = [toFormula e]
          toName (Single _ (Just n)) = [n]
          toName (Range e1 e2) = slice (pos e1) (pos e2) $ fnames rst
          toName AllCols = fromMaybe [] (names rst)

processRow :: [Col] -> Row -> Row
processRow cs r = concatMap f cs
    where f (Single e _) = [evaluate r [] e]
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
