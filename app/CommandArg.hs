module CommandArg where

parseArgs :: [String] -> [(String, String)]
parseArgs [] = []
parseArgs (('-' : arg) : []) = [(arg, "")]
parseArgs (('-' : arg1) : ('-' : arg2) : args) = [(arg1, "")] ++ (parseArgs (('-' : arg2) : args))
parseArgs (('-' : arg) : value : args) = [(arg, value)] ++ (parseArgs args)
parseArgs (arg : args) = [("", arg)] ++ (parseArgs args)


