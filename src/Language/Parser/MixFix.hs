{-# LANGUAGE DeriveFunctor #-}
module Language.Parser.MixFix where

import Control.Applicative
import Numeric.Natural

data Associativity = LeftA | RightA | NonA
  deriving (Eq, Show)

data Fixity = Prefix | Postfix | Infix Associativity | Closed
  deriving (Eq, Show)

data Operator = Operator { fixity :: Fixity, arity :: Natural, nameParts :: [String] }
  deriving (Eq, Show)

ifThenElse :: Operator
ifThenElse = Operator Prefix 2 [ "if", "then", "else" ]

data Precedence = Precedence { operators :: Fixity -> [Operator], successorNodes :: [Precedence] }

type PrecedenceGraph = [Precedence]

data Parser a = Null | Pure a
  deriving (Eq, Show, Functor)

instance Applicative Parser where
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  _ <*> _ = Null

instance Alternative Parser where
  empty = Null
  Null <|> r = r
  l <|> _ = l
