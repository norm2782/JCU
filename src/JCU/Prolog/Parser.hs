{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module JCU.Prolog.Parser where

import JCU.Prolog.Types
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Data.ListLike.Base (ListLike)

pRules :: Parser [Rule]
pRules = pList pRule

pRule :: Parser Rule
pRule = (:<-:) <$> pFun  <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural
pVar   =  Var   <$>  lexeme (pList1 pUpper)
pFun   =  Fun   <$>  pIdentifier <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms = pListSep pComma pTerm 

startParse :: (ListLike s b, Show b) => P (Str b s LineColPos) a -> s
                                     -> (a, [Error LineColPos])
startParse p inp = parse ((,) <$> p <*> pEnd)
                 $ createStr (LineColPos 0 0 0) inp

pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> lexeme (pList (pLower <|> pUpper <|> pDigit))

