{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module JCU.Prolog.Parser where

import JCU.Prolog.Types
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

pRules :: Parser [Rule]
pRules = pList pRule

pRule :: Parser Rule
pRule = (:<-:) <$> pFun  <*> ((pSpaces *> pToken ":-" <* pSpaces *> pTerms) `opt` [])
                         <* pSpaces <* pDot

pTerm, pCon, pVar, pFun :: Parser Term
pTerm  =  pCon  <|>  pVar <|> pFun
pCon   =  Con   <$>  pNatural <* pSpaces
pVar   =  Var   <$>  pList1 pUpper <* pSpaces
pFun   =  Fun   <$>  (pIdentifier <* pSpaces) <*> (pParens pTerms `opt` [])

pTerms :: Parser [Term]
pTerms = pListSep pComma (pSpaces *> pTerm <* pSpaces)

startParse :: Parser a -> String -> (a, [Error LineColPos])
startParse p inp = parse ((,) <$> p <*> pEnd) $ createStr (LineColPos 0 0 0) inp

pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> pList (pLower <|> pUpper <|> pDigit)
                             <* pSpaces

