-- File auto generated by purescript-bridge! --
module Types where

import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Array, Int, String)

import Prelude

newtype Foo =
    Foo {
      _fooMessage :: String
    , _fooNumber :: Int
    , _fooList :: Array Int
    }

derive instance genericFoo :: Generic Foo _
derive instance newtypeFoo :: Newtype Foo _

--------------------------------------------------------------------------------
_Foo :: Iso' Foo { _fooMessage :: String, _fooNumber :: Int, _fooList :: Array Int}
_Foo = _Newtype

fooMessage :: Lens' Foo String
fooMessage = _Newtype <<< prop (SProxy :: SProxy "_fooMessage")

fooNumber :: Lens' Foo Int
fooNumber = _Newtype <<< prop (SProxy :: SProxy "_fooNumber")

fooList :: Lens' Foo (Array Int)
fooList = _Newtype <<< prop (SProxy :: SProxy "_fooList")

--------------------------------------------------------------------------------
