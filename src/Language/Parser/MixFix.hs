module Language.Parser.MixFix where

import Numeric.Natural

data Associativity = LeftA | RightA | NonA
  deriving (Eq, Show)

data Fixity = Prefix | Postfix | Infix Associativity | Closed
  deriving (Eq, Show)

data Operator = Operator { fixity :: Fixity, arity :: Natural, nameParts :: [String] }
  deriving (Eq, Show)

ifThenElse :: Operator
ifThenElse = Operator Prefix 2 [ "if", "then", "else" ]

data Precedence = Precedence (Fixity -> [Operator]) [Precedence]

type PrecedenceGraph = [Precedence]

operators :: Precedence -> Fixity -> [Operator]
operators (Precedence o s) = o

successorNodes :: Precedence -> [Precedence]
successorNodes (Precedence o s) = s
