{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge.Tuple where

import qualified Data.Text as T
import           Language.PureScript.Bridge.Builder (BridgePart, doCheck, (^==))
import           Language.PureScript.Bridge.PSTypes (psTuple)
import           Language.PureScript.Bridge.TypeInfo (HasHaskType (haskType),
                                                      HaskellType,
                                                      TypeInfo (_typeName), typeName)

tupleBridge :: BridgePart
#if __GLASGOW_HASKELL__>=908
tupleBridge = typeName ^== "Tuple2" >> psTuple
#else
tupleBridge = doCheck haskType isTuple >> psTuple
#endif

data TupleParserState = Start | OpenFound | ColonFound | Tuple | NoTuple
  deriving (Eq, Show)

step :: TupleParserState -> Char -> TupleParserState
step Start '('      = OpenFound
step Start _        = NoTuple
step OpenFound ','  = ColonFound
step OpenFound _    = NoTuple
step ColonFound ',' = ColonFound
step ColonFound ')' = Tuple
step ColonFound _   = NoTuple
step Tuple _        = NoTuple
step NoTuple _      = NoTuple

isTuple :: HaskellType -> Bool
isTuple = (== Tuple) . T.foldl' step Start . _typeName
