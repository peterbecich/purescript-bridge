{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.PureScript.Bridge.Primitives where

import           Control.Monad.Reader.Class (MonadReader)
import           Language.PureScript.Bridge.Builder (BridgeData, BridgePart,
                                                     clearPackageFixUp, (<|>),
                                                     (^==))
import           Language.PureScript.Bridge.PSTypes (psArray, psBool, psEither,
                                                     psInt, psMap, psMaybe,
                                                     psNumber, psObject, psSet,
                                                     psString, psUnit, psWord,
                                                     psWord16, psWord32,
                                                     psWord64, psWord8)
import           Language.PureScript.Bridge.TypeInfo (HasHaskType (haskType),
                                                      PSType, mkTypeInfo,
                                                      typeModule, typeName)

boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

strMapBridge :: BridgePart
strMapBridge = typeName ^== "Map" >> psObject

setBridge :: BridgePart
setBridge = do
    typeName ^== "Set"
    typeModule ^== "Data.Set" <|> typeModule ^== "Data.Set.Internal"
    psSet

mapBridge :: BridgePart
mapBridge = do
    typeName ^== "Map"
    typeModule ^== "Data.Map" <|> typeModule ^== "Data.Map.Internal"
    psMap

-- | Dummy bridge, translates every type with 'clearPackageFixUp'
dummyBridge :: (MonadReader BridgeData m) => m PSType
dummyBridge = clearPackageFixUp

intBridge :: BridgePart
intBridge = typeName ^== "Int" >> return psInt

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

listBridge :: BridgePart
#if __GLASGOW_HASKELL__>=906
listBridge = typeName ^== "List" >> psArray
#else
listBridge = typeName ^== "[]" >> psArray
#endif

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge =
    haskType ^== mkTypeInfo @String >> return psString

textBridge :: BridgePart
textBridge = do
    typeName ^== "Text"
    typeModule ^== "Data.Text.Internal"
        <|> typeModule ^== "Data.Text.Internal.Lazy"
    return psString

unitBridge :: BridgePart
unitBridge =
  typeName ^== "()" <|> typeName ^== "Unit" >> return psUnit

noContentBridge :: BridgePart
noContentBridge = typeName ^== "NoContent" >> return psUnit

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> return psWord

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> return psWord8

word16Bridge :: BridgePart
word16Bridge = typeName ^== "Word16" >> return psWord16

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> return psWord32

word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> return psWord64
