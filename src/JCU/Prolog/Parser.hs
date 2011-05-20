{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module JCU.Prolog.Parser where

import            Data.ListLike.Base (ListLike)
import            JCU.Prolog.Types
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils

pRules :: Parser [Rule]
pRules = pList pRule

pRule :: Parser Rule
pRule = (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerm, pVar, pFun :: Parser Term
pTerm  = pVar  <|>  pFun
pVar   = Var   <$>  lexeme (pList1 pUpper)
pFun   = Fun   <$>  pLowerCase <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms = pListSep pComma pTerm

startParse :: (ListLike s b, Show b)  => P (Str b s LineColPos) a -> s
                                      -> (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pLowerCase :: Parser String
pLowerCase = (:) <$> pLower <*> lexeme (pList (pLower <|> pUpper <|> pDigit))
