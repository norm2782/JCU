{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Types where

import            Control.Applicative ((<*>), (<$>), (<|>))
import            Control.Monad (mzero)
import            Data.Aeson (  ToJSON(..), FromJSON(..), (.=), object,
                                Value(..), (.:))
import            Data.ByteString (ByteString)
import            Snap.Auth (AuthUser)
import            JCU.Prolog.Types (Term(..), Rule(..), Trace(..))

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: ByteString }

{- instance ToJSON Term where-}
{-   toJSON (Con number)       = object  [  "number"  .= number]-}
{-   toJSON (Var ident)        = object  [  "ident"   .= ident]-}
{-   toJSON (Fun ident terms)  = object  [  "ident"   .= ident-}
{-                                       ,  "terms"   .= map toJSON terms ]-}

{- instance ToJSON Rule where-}
{-   toJSON (term :<-: terms) = object  [  "term"   .= toJSON term-}
{-                                      ,  "terms"  .= map toJSON terms ]-}

{- instance ToJSON Trace where-}
{-   toJSON (Trace g u e ts) = object  [  "goal"   .= toJSON g-}
{-                                     ,  "unif"   .= toJSON u-}
{-                                     ,  "env"    .= toJSON e-}
{-                                     ,  "terms"  .= map toJSON ts ]-}

{- instance FromJSON Term where-}
{-   parseJSON (Object t)  =  Con  <$> t .: "number"-}
{-                       <|>  Var  <$> t .: "ident"-}
{-                       <|>  Fun  <$> t .: "ident" <*> t .: "terms"-}
{-   parseJSON _           = mzero-}

{- instance FromJSON Rule where-}
{-   parseJSON (Object t)  = (:<-:) <$> t .: "term" <*> t .: "terms"-}
{-   parseJSON _           = mzero-}

{- instance FromJSON Trace where-}
{-   parseJSON (Object t)  = Trace  <$>  t .: "goal" <*> t .: "unif"-}
{-                                  <*>  t .: "env" <*> t .: "terms"-}
{-   parseJSON _           = mzero-}

instance ToJSON Rule where
  toJSON t = object [ "rule" .= show t ]
