{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Prelude

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Constraint (Constraint)
import Data.Proxy
import Data.Text
import GHC.Generics
import GHC.TypeLits
    (ErrorMessage (Text), KnownSymbol, Symbol, TypeError, symbolVal)
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

import           Deriving.Aeson (CustomJSON (..))
import qualified Deriving.Aeson as DA

data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''Foo

fooProxy :: Proxy Foo
fooProxy = Proxy

-- https://discourse.purescript.org/t/decode-untagged-json-with-argonaut-generic-library/2102/4
-- Credit to: https://discourse.purescript.org/u/and-pete

data ConstTag (tag :: Symbol)

type MyWrapperType tag = CustomJSON
  '[ DA.ConstructorTagModifier (ConstTag tag)
   , DA.SumTaggedObject "tag" "contents"
   , DA.TagSingleConstructors
   ]

type MyInnerRecordType labelPrefix = CustomJSON
  '[ DA.SumObjectWithSingleField
  , DA.ConstructorTagModifier DA.CamelToSnake
  , DA.FieldLabelModifier (DA.StripPrefix labelPrefix, DA.CamelToSnake)
  ]

type family NonEmptyString (xs :: Symbol) :: Constraint where
  NonEmptyString "" = TypeError ('Text "Empty string provided for ConstTag constructor tag")
  NonEmptyString _  = ()

instance (KnownSymbol tag, NonEmptyString tag) => DA.StringModifier (ConstTag tag) where
  getStringModifier = const string
    where
      string = symbolVal (Proxy @tag)

newtype Wrapper (tag::Symbol) a = Wrapper a
  deriving ( Show, Eq, Generic )
  deriving ( FromJSON, ToJSON )
    via MyWrapperType tag (Wrapper tag a)

data Bar = Bar
  { _barMessage :: Text
  , _barNumber  :: Int
  } deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON)
      via Wrapper "bar" (MyInnerRecordType "_bar" Bar)

barProxy :: Proxy Bar
barProxy = Proxy

makeLenses ''Bar

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy Foo)
  , mkSumType (Proxy :: Proxy Bar)
  ]
