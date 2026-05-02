module SimpleType where

data SimpleType = AtomicType String | FunctionType SimpleType SimpleType

simpleTypeEq :: SimpleType -> SimpleType -> Bool
simpleTypeEq (AtomicType name1) (AtomicType name2) = name1 == name2
simpleTypeEq (FunctionType functionType1SimpleType1 functionType1SimpleType2) (FunctionType functionType2SimpleType1 functionType2SimpleType2) =
    simpleTypeEq functionType1SimpleType1 functionType2SimpleType1 && simpleTypeEq functionType1SimpleType2 functionType2SimpleType2
simpleTypeEq _ _ = False

instance Show SimpleType where
    show (AtomicType name) = name
    show (FunctionType simpleType1 simpleType2) = "(" ++ show simpleType1 ++ "->" ++ show simpleType2 ++ ")"

