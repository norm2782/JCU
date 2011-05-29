{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module JCU.Types where

import            Control.Applicative
import            Data.Aeson as AE
import            Data.Attoparsec.Lazy as AT (Result(..), parse)
import            Data.ByteString (ByteString)
import qualified  Data.ByteString.Lazy.Char8 as L (unpack, ByteString)
import            Data.Tree (Tree(..))
import            Language.Prolog.NanoProlog.NanoProlog
import            Snap.Auth (AuthUser)
import            Text.ParserCombinators.UU.BasicInstances (Parser())
import Debug.Trace

data User     =  User  {  authUser     :: AuthUser
                       ,  storedRules  :: [ByteString] }
              deriving Show

data DropReq  =  DropReq Term Rule Proof Int
              deriving Show

data DropRes  =  DropRes Bool Int [Term] Proof
              deriving Show

data Status   =  Correct
              |  Incomplete
              |  Invalid
              deriving Show

type Proof    =  Tree Term
type PCheck   =  Tree Status
type Cid      =  String

instance FromJSON DropReq where
  parseJSON (Object o)  = mkJSONDropReq  <$>  o .: "term"
                                         <*>  o .: "rule"
                                         <*>  o .: "proof"
                                         <*>  o .: "treeLvl"
  parseJSON val         = fail $ "No case for (FromJSON DropReq) with value: " ++ show val

mkJSONDropReq :: String -> String -> Value -> Int -> DropReq
mkJSONDropReq tm rl prf lvl = DropReq (mkJSONTerm tm) (mkJSONRule rl) mkProofTree lvl
  where mkProofTree = case fromJSON prf :: AE.Result Proof of
                        (Success a)  -> a
                        (Error err)  -> error ("Error parsing drop request: " ++ err)

instance ToJSON DropRes where
  toJSON (DropRes ufd cs uts prf) = object  [  "unified"   .= ufd
                                            ,  "children"  .= cs
                                            ,  "urhss"     .= map show uts
                                            ,  "nproof"    .= prf ]

instance ToJSON PCheck where
  toJSON (Node st cs) = object  [  "proofCheckResult"    .= show st
                                ,  "proofCheckChildren"  .= toJSON cs ]

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]

instance FromJSON Rule where
  parseJSON (Object o)  = mkJSONRule <$> o .: "rule"
  parseJSON val         = fail $ "No case for (FromJSON Rule) with value: " ++ show val

-- TODO: Errors
mkJSONRule :: String -> Rule
mkJSONRule = fst . startParse pRule

instance FromJSON Proof where
  parseJSON (Object o)  = mkJSONProofTree <$> o .: "term" <*> o .: "childTerms"
  parseJSON val         = fail $ "No case for (FromJSON Proof) with value: " ++ show val

instance ToJSON Proof where
  toJSON (Node t ps) = object  [  "term"        .= show t
                               ,  "childTerms"  .= toJSON ps ]

mkJSONProofTree :: String -> Value -> Proof
mkJSONProofTree tm rts = Node (mkJSONTerm tm) mkProofTrees
  where mkProofTrees = case fromJSON rts :: AE.Result [Proof] of
                         (Success a)  -> a
                         (Error err)  -> error ("Error parsing proof tree: " ++ err)

-- TODO: Something with errors
mkJSONTerm :: String -> Term
mkJSONTerm = fst . startParse pTerm

mkRule :: L.ByteString -> Either String Rule
mkRule = processJSON fromJSON

mkDropReq :: L.ByteString -> Either String DropReq
mkDropReq = processJSON fromJSON

mkProof :: L.ByteString -> Either String Proof
mkProof = processJSON fromJSON

processJSON :: (Value -> AE.Result a) -> L.ByteString -> Either String a
processJSON f raw =
  case AT.parse json raw of
    (AT.Done _ r)  ->
      case f r of
        (Success a)  -> Right a
        (Error err)  -> Left $ "Error converting ByteString to data type: " ++ err
    (AT.Fail _ _ err) -> Left $ "Error parsing raw JSON: " ++ err

-- TODO: Try to get rid of the explicit annotations...
parseCheck :: Maybe ByteString -> L.ByteString -> (Bool, [String])
parseCheck Nothing   _     = checkErr ["Unknown error." :: String]
parseCheck (Just x)  body
  | x == "rule"  = parseMsg pRule body
  | x == "term"  = parseMsg pTerm body
  | otherwise    = checkErr ["Invalid type specified" :: String]
  where  parseMsg :: Parser t -> L.ByteString -> (Bool, [String])
         parseMsg p txt = writeRes $ startParse p (L.unpack txt)
         writeRes :: Show a => (t, [a]) -> (Bool, [String])
         writeRes (_, [])  = (True, [""])
         writeRes (_, rs)  = checkErr rs

checkErr :: Show a => [a] -> (Bool, [String])
checkErr msg = (False, map show msg)
