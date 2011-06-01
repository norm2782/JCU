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

data User      =  User  {  authUser     :: AuthUser
                        ,  storedRules  :: [ByteString] }
               deriving Show

data DropReq   =  DropReq Proof [Int] Rule
               deriving Show

data DropRes   =  DropRes Bool Proof
               deriving Show

data Status    =  Correct
               |  Incomplete
               |  Invalid
               deriving Show

type Proof     =  Tree Term
type PCheck    =  Tree Status
type ErrorMsg  =  String

instance FromJSON DropReq where
  parseJSON (Object o)  = mkJSONDropReq  <$>  o .: "proof"
                                         <*>  o .: "treeLvl"
                                         <*>  o .: "rule"

  parseJSON val         = fail $ "No case for (FromJSON DropReq) with value: " ++ show val

mkJSONDropReq :: Value -> [Int] -> String -> DropReq
mkJSONDropReq prf lvl rl = DropReq mkProofTree lvl (mkJSONRule rl)
  where mkProofTree = case fromJSON prf :: AE.Result Proof of
                        (Success a)  -> a
                        (Error err)  -> error ("Error parsing drop request: " ++ err)

instance ToJSON DropRes where
  toJSON (DropRes ufd prf) = object ["unified" .= ufd, "nproof" .= prf]

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

mkRule :: L.ByteString -> Either ErrorMsg Rule
mkRule = processJSON fromJSON

mkDropReq :: L.ByteString -> Either ErrorMsg DropReq
mkDropReq = processJSON fromJSON

mkProof :: L.ByteString -> Either ErrorMsg Proof
mkProof = processJSON fromJSON

processJSON :: (Value -> AE.Result a) -> L.ByteString -> Either ErrorMsg a
processJSON f raw =
  case AT.parse json raw of
    (AT.Done _ r)  ->
      case f r of
        (Success a)  -> Right a
        (Error err)  -> Left $ "Error converting ByteString to data type: " ++ err
    (AT.Fail _ _ err) -> Left $ "Error parsing raw JSON: " ++ err

-- TODO: Try to get rid of the explicit annotations...
parseCheck :: Maybe ByteString -> L.ByteString -> (Bool, [ErrorMsg])
parseCheck Nothing   _     = checkErr ["Unknown error." :: ErrorMsg]
parseCheck (Just x)  body
  | x == "rule"  = parseMsg pRule body
  | x == "term"  = parseMsg pTerm body
  | otherwise    = checkErr ["Invalid type specified" :: ErrorMsg]
  where  parseMsg :: Parser t -> L.ByteString -> (Bool, [ErrorMsg])
         parseMsg p txt = writeRes $ startParse p (L.unpack txt)
         writeRes :: Show a => (t, [a]) -> (Bool, [ErrorMsg])
         writeRes (_, [])  = (True, [""])
         writeRes (_, rs)  = checkErr rs

checkErr :: Show a => [a] -> (Bool, [String])
checkErr msg = (False, map show msg)
