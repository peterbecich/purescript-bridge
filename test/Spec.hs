{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Word (Word, Word64)
import           Language.PureScript.Bridge (CustomInstance (CustomInstance),
                                             Instance (Custom),
                                             InstanceImplementation (Derive, DeriveNewtype, Explicit),
                                             InstanceMember (InstanceMember),
                                             Language (Haskell, PureScript),
                                             Modules, SumType (..),
                                             TypeInfo (TypeInfo, _typeModule, _typeName, _typePackage, _typeParameters),
                                             bridgeSumType, buildBridge,
                                             defaultBridge, equal, equal1,
                                             functor, genericShow, mkSumType,
                                             mkTypeInfo, moduleToText, order,
                                             renderText, sumTypeToDocs,
                                             sumTypeToModule)
import           Language.PureScript.Bridge.TypeParameters (A, B, C, M1)
import           NeatInterpolation (trimming, untrimming)
import qualified RoundTripArgonautAesonGeneric.Spec (roundtripSpec)
import qualified RoundTripJsonHelpers.Spec (roundtripSpec)
import           Test.Hspec (Spec, describe, hspec, it)
import           Test.Hspec.Expectations.Pretty (Expectation, shouldBe)
import           TestData (Bar, Foo, Func, Simple, SingleProduct, SingleRecord,
                           SingleValueConstr, SomeNewtype, WeekInMonth,
                           weekInMonth)
import           Text.PrettyPrint.Leijen.Text (Doc, cat, linebreak, punctuate,
                                               vsep)

-- | There are RoundTrip tests for both the `json-helpers` library and the
-- `argonaut-aeson-generic` library.
-- These have been separated into two directories, and the test specs are duplicated.
-- This is necessary because the generated PureScript takes its module name from the
-- Haskell module which generates it.
-- If both generated PureScript modules have the same module name, the root
-- Spago project will fail to build because of module name duplication.
main :: IO ()
main =
  hspec $ allTests
  *> RoundTripArgonautAesonGeneric.Spec.roundtripSpec
  *> RoundTripJsonHelpers.Spec.roundtripSpec

custom :: SumType 'Haskell -> SumType 'Haskell
custom (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance [] (TypeInfo "" "Data.MyClass" "MyClass" [TypeInfo "" "" "Foo" []]) $
                Explicit
                    [ InstanceMember "member1" ["foo", "bar"] "undefined" [] mempty
                    , InstanceMember "member2" [] "do\npure unit" [] mempty
                    ]

customNewtypeDerived :: SumType 'Haskell -> SumType 'Haskell
customNewtypeDerived (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance
                [TypeInfo "" "" "Eq" [TypeInfo "" "" "Foo" []]]
                (TypeInfo "" "Data.MyNTClass" "MyNTClass" [TypeInfo "" "" "Foo" []])
                DeriveNewtype

customDerived :: SumType 'Haskell -> SumType 'Haskell
customDerived (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance
                [ TypeInfo "" "" "Eq" [TypeInfo "" "" "Foo" []]
                , TypeInfo "" "" "Show" [TypeInfo "" "" "Foo" []]
                ]
                (TypeInfo "" "Data.MyDClass" "MyDClass" [TypeInfo "" "" "Foo" []])
                Derive

allTests :: Spec
allTests = do
    describe "buildBridge without lens-code-gen" $ do
        it "week in month" $ do
            let sumType =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (mkSumType @WeekInMonth)
                doc = vsep $ sumTypeToDocs sumType
                txt = [trimming|
                  data WeekInMonth
                    = WeekFirst
                    | WeekSecond
                    | WeekThird
                    | WeekFourth
                    | WeekLast

                  derive instance Generic WeekInMonth _

                  derive instance Eq WeekInMonth

                  derive instance Ord WeekInMonth

                  instance Enum WeekInMonth where
                    succ = genericSucc
                    pred = genericPred

                  instance Bounded WeekInMonth where
                    bottom = genericBottom
                    top = genericTop
                  |]
             in doc `shouldRender` txt
        it "tests generation of custom typeclasses" $
            let sumType =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (customNewtypeDerived . customDerived . custom $ mkSumType @Foo)
                doc = vsep $ sumTypeToDocs sumType
                txt = [trimming|
                        data Foo
                          = Foo
                          | Bar Int
                          | FooBar Int String

                        derive newtype instance (Eq Foo) => MyNTClass Foo

                        derive instance (Eq Foo, Show Foo) => MyDClass Foo

                        instance MyClass Foo where
                          member1 foo bar = undefined
                          member2 = do
                            pure unit

                        derive instance Generic Foo _
                        |]
             in doc `shouldRender` txt
        it "tests generation of typeclasses for custom type Foo" $
            let sumType =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (genericShow . order $ mkSumType @Foo)
                doc = vsep $ sumTypeToDocs sumType
                txt = [trimming|
                        data Foo
                          = Foo
                          | Bar Int
                          | FooBar Int String

                        instance Show Foo where
                          show a = genericShow a

                        derive instance Eq Foo

                        derive instance Ord Foo

                        derive instance Generic Foo _
                        |]
             in doc `shouldRender` txt
        it "tests generation of typeclasses for custom type Func" $
            let sumType =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (equal1 . functor . genericShow $ mkSumType @(Func A))
                doc = vsep $ sumTypeToDocs sumType
                txt = [trimming|
                        data Func a = Func Int a

                        derive instance Eq1 Func

                        derive instance Functor Func

                        instance (Show a) => Show (Func a) where
                          show a = genericShow a

                        derive instance Generic (Func a) _
                        |]
             in doc `shouldRender` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' :: SumType 'PureScript
                advanced' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (mkSumType @(Bar A B M1 C))
                modules :: Modules
                modules = sumTypeToModule Nothing advanced'
                m = head . map (moduleToText) . Map.elems $ modules
                txt =
                  [untrimming| -- File auto generated by purescript-bridge! --
                               module TestData where

                               import Prelude

                               import Data.Either (Either)
                               import Data.Generic.Rep (class Generic)
                               import Data.Maybe (Maybe(..))
                               import Data.Newtype (class Newtype)

                               data Bar a b m c
                                 = Bar1 (Maybe a)
                                 | Bar2 (Either a b)
                                 | Bar3 a
                                 | Bar4 { myMonadicResult :: m b }

                               derive instance Generic (Bar a b m c) _|]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                        newtype SingleRecord a b = SingleRecord
                          { _a :: a
                          , _b :: b
                          , c :: String
                          }

                        derive instance Generic (SingleRecord a b) _

                        derive instance Newtype (SingleRecord a b) _
                        |]
             in doc `shouldRender` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (mkSumType @SomeNewtype)
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                        newtype SomeNewtype = SomeNewtype Int

                        derive instance Generic SomeNewtype _

                        derive instance Newtype SomeNewtype _
                        |]
             in doc `shouldRender` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (mkSumType @SingleValueConstr)
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                        newtype SingleValueConstr = SingleValueConstr Int

                        derive instance Generic SingleValueConstr _

                        derive instance Newtype SingleValueConstr _
                        |]
             in doc `shouldRender` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType' =
                  bridgeSumType
                      (buildBridge defaultBridge)
                      (mkSumType @SingleProduct)
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                      data SingleProduct = SingleProduct String Int

                      derive instance Generic SingleProduct _
                      |]
             in doc `shouldRender` txt
        it "tests generation Eq instances for polymorphic types" $
            let recType' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (equal $ mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                        newtype SingleRecord a b = SingleRecord
                          { _a :: a
                          , _b :: b
                          , c :: String
                          }

                        derive instance (Eq a, Eq b) => Eq (SingleRecord a b)

                        derive instance Generic (SingleRecord a b) _

                        derive instance Newtype (SingleRecord a b) _
                        |]
             in doc `shouldRender` txt
        it "tests generation of Ord instances for polymorphic types" $
            let recType' =
                    bridgeSumType
                        (buildBridge defaultBridge)
                        (order $ mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt = [trimming|
                        newtype SingleRecord a b = SingleRecord
                          { _a :: a
                          , _b :: b
                          , c :: String
                          }

                        derive instance (Eq a, Eq b) => Eq (SingleRecord a b)

                        derive instance (Ord a, Ord b) => Ord (SingleRecord a b)

                        derive instance Generic (SingleRecord a b) _

                        derive instance Newtype (SingleRecord a b) _
                        |]
             in doc `shouldRender` txt

    describe "tests bridging Haskells Data.Word to PureScripts Data.Word from purescript-word" $ do
        describe "moduleToText" $
            it "should contain the right import and datatype" $ do
                let createModuleText :: SumType 'Haskell -> T.Text
                    createModuleText sumType =
                        let bridge = buildBridge defaultBridge
                            modules = sumTypeToModule Nothing (bridgeSumType bridge sumType)
                         in head . map moduleToText . Map.elems $ modules
                    expectedText =
                      [untrimming| -- File auto generated by purescript-bridge! --
                                   module TestData where

                                   import Prelude

                                   import Data.Generic.Rep (class Generic)
                                   import Data.Lens.Iso.Newtype (_Newtype)
                                   import Data.Lens.Record (prop)
                                   import Data.Maybe (Maybe(..))
                                   import Data.Newtype (class Newtype)
                                   import Data.Word (Word64)
                                   import Type.Proxy (Proxy(Proxy))

                                   newtype Simple Word64 = Simple Word64

                                   derive instance Generic (Simple Word64) _

                                   derive instance Newtype (Simple Word64) _|]

                createModuleText (mkSumType @(Simple Word64)) `shouldBe` expectedText
        describe "buildBridge" $
            it "should create the correct type information for Word" $ do
                let bst = buildBridge defaultBridge (mkTypeInfo @Word)
                    ti =
                        TypeInfo
                            { _typePackage = "purescript-word"
                            , _typeModule = "Data.Word"
                            , _typeName = "Word"
                            , _typeParameters = []
                            }
                 in bst `shouldBe` ti

shouldRender :: Doc -> T.Text -> Expectation
shouldRender actual expected = renderText actual `shouldBe` T.stripEnd expected
