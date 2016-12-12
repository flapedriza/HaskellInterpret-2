import           System.IO
import           System.Random

type Ident = String


---------------------------------------------------------------------------------
---------------------------COMMAND DATA TYPE DEFINITION--------------------------
---------------------------------------------------------------------------------
data Command a =
    Assign Ident (NExpr a)
    |Input Ident
    |Print Ident
    |Empty Ident
    |Push Ident (NExpr a)
    |Pop Ident Ident
    |Size Ident Ident
    |Seq [Command a]
    |Cond (BExpr a) (Command a) (Command a)
    |Loop (BExpr a) (Command a) deriving (Read)

instance (Show a) => Show (Command a) where
    show (Assign v n) = v ++ " := " ++ show n ++ "\n"
    show (Input v)    = "INPUT " ++ v ++ "\n"
    show (Print v)    = "PRINT " ++ v ++ "\n"
    show (Empty v)    = "EMPTY " ++ v ++ "\n"
    show (Push v n)   = "PUSH " ++ v ++ " " ++ show n ++ "\n"
    show (Pop v v2)   = "POP " ++ v ++ " " ++ v2 ++ "\n"
    show (Size v v2)  = "SIZE " ++ v ++ " " ++ v2 ++ "\n"
    show (Seq [])     = ""
    show (Seq (x:xs)) = show x ++ show (Seq xs)
    show (Loop b c)   = "WHILE " ++ show b ++ "\nDO\n" ++ indent c ++ "END\n"
    show (Cond b c1 c2) =
        "IF " ++ show b ++ " THEN\n" ++ indent c1 ++ "ELSE\n"
        ++ indent c2 ++ "END\n"


indent :: (Show a) => Command a -> String
indent c = printall $ lines (show c)
    where
        printall []     = ""
        printall (x:xs) = "  " ++ x ++ "\n" ++ printall xs

---------------------------------------------------------------------------------
--------------------EVALUABLE EXPRESSIONS DATA TYPE DEFINITION-------------------
---------------------------------------------------------------------------------
class Evaluable e where
    eval :: (Num a, Ord a) => (Ident -> Maybe a) -> e a -> Either String a
    typeCheck :: (Ident -> String) -> e a -> Bool


data BExpr a =
    AND (BExpr a) (BExpr a)
    |OR (BExpr a) (BExpr a)
    |NOT (BExpr a)
    |Gt (NExpr a) (NExpr a)
    |Eq (NExpr a) (NExpr a) deriving (Read)

instance (Show a) => Show (BExpr a) where
    show (AND b1 b2) = show b1 ++ " AND " ++ show b2
    show (OR b1 b2)  = show b1 ++ " OR " ++ show b2
    show (NOT b)     = "NOT " ++ show b
    show (Gt a b)    = show a ++ " > " ++ show b
    show (Eq a b)    = show a ++ " = " ++ show b

instance Evaluable BExpr where
    eval varfind (AND a b) = case eval varfind a of
        Left err -> Left err
        Right a1 -> case eval varfind b of
            Left err -> Left err
            Right b1 -> Right (if a1+b1 == 2 then 1 else 0)

    eval varfind (OR a b) = case eval varfind a of
        Left err -> Left err
        Right a1 -> case eval varfind b of
            Left err -> Left err
            Right b1 -> Right (if a1+b1 > 0 then 1 else 0)

    eval varfind (NOT a) = case eval varfind a of
        Left err   -> Left err
        Right bool -> Right (if bool == 1 then 0 else 1)

    eval varfind (Gt a b) = case eval varfind a of
        Left err   -> Left err
        Right numa -> case eval varfind b of
            Left err   -> Left err
            Right numb -> Right (if numa > numb then 1 else 0)

    eval varfind (Eq a b) = case eval varfind a of
        Left err   -> Left err
        Right numa -> case eval varfind b of
            Left err   -> Left err
            Right numb -> Right (if numa == numb then 1 else 0)

data NExpr a =
    Var Ident
    |Const a
    |Plus (NExpr a) (NExpr a)
    |Minus (NExpr a) (NExpr a)
    |Times (NExpr a) (NExpr a) deriving (Read)

instance (Show a) => Show (NExpr a) where
    show (Const a)   = show a
    show (Var v)     = v
    show (Plus a b)  = show a ++ " + " ++ show b
    show (Minus a b) = show a ++ " - " ++ show b
    show (Times a b) = show a ++ " * " ++ show b

instance Evaluable NExpr where
    eval varfind (Var name) = case varfind name of
        Just n  -> Right n
        Nothing -> Left "Undefined variable"

    eval _ (Const n) = Right n

    eval varfind (Plus a b) = case eval varfind a of
        Left err -> Left err
        Right n  -> case eval varfind b of
            Left err -> Left err
            Right n2 -> Right (n + n2)

    eval varfind (Minus a b) = case eval varfind a of
        Left err -> Left err
        Right n  -> case eval varfind b of
            Left err -> Left err
            Right n2 -> Right (n - n2)

    eval varfind (Times a b) = case eval varfind a of
        Left err -> Left err
        Right n  -> case eval varfind b of
            Left err -> Left err
            Right n2 -> Right (n * n2)

---------------------------------------------------------------------------------
---------------------------SYMTABLE DATA TYPE DEFINITION-------------------------
---------------------------------------------------------------------------------
data SymTable a =
    EmptyTable
    |Node (String, Either a [a]) (SymTable a) (SymTable a)

instance (Show a) => Show (SymTable a) where
    show table = show (preOrder table)
        where
            preOrder EmptyTable = []
            preOrder (Node (a,b) left right) = (a,b) : preOrder left ++ preOrder right

update :: SymTable a -> Ident -> Either a [a] -> SymTable a
update EmptyTable name val = Node (name, val) EmptyTable EmptyTable
update (Node (a,b) left right) name val
    |name == a = Node (a, val) left right
    |name < a  = Node (a,b) (update left name val) right
    |otherwise = Node (a,b) left (update right name val)

numValue :: SymTable a -> Ident -> Maybe a
numValue EmptyTable _ = Nothing
numValue (Node (a,b) left right) name
    |name == a = case b of
        Left val -> Just val
        Right _  -> Nothing
    |name < a  = numValue left name
    |otherwise = numValue right name

stackValue :: SymTable a -> Ident -> Maybe [a]
stackValue EmptyTable _ = Nothing
stackValue (Node (a,b) left right) name
    |name == a = case b of
        Left _      -> Nothing
        Right stack -> Just stack
    |name < a  = stackValue left name
    |otherwise = stackValue right name

---------------------------------------------------------------------------------
---------------------------------PROGRAM EXECUTION-------------------------------
---------------------------------------------------------------------------------


interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> (Either String [a], SymTable a, [a])
--Assign
interpretCommand m i (Assign name expr) = case eval (numValue m) expr of
    Left err -> (Left err, m, i)
    Right n  -> (Right [], update m name (Left n), i)
--Input
interpretCommand m (x:input) (Input name) = (Right [], update m name (Left x), input)
--Print
interpretCommand m i (Print name) = case eval (numValue m) (Var name) of
    Left err -> (Left err, m, i)
    Right n  -> (Right [n], m, i)
--Empty
interpretCommand m i (Empty name) = (Right [], update m name (Right []), i)
--Push
interpretCommand m i (Push name expr) = case eval (numValue m) expr of
    Left err -> (Left err, m, i)
    Right n  -> case stackValue m name of
        Nothing -> (Left "Undefined variable", m, i)
        Just xs -> (Right [], update m name (Right (n:xs)), i)
--Pop
interpretCommand m i (Pop name var) = case stackValue m name of
    Nothing -> (Left "Undefined variable", m, i)
    Just xs -> if not (null xs) then let updm = update m var (Left (head xs)) in (Right [], update updm name (Right (tail xs)), i) else (Left "Empty stack", m, i)
--size
interpretCommand m i (Size name var) = case stackValue m name of
    Nothing -> (Left "Undefined variable", m, i)
    Just xs -> (Right [], update m var (Left (fromIntegral $ length xs)), i)
--Seq
interpretCommand m i (Seq comms) = case processList (genList m i comms) of
    Left err             -> (Left err, m, i)
    Right (out, mem, i2) -> (Right out, mem, i2)
    where
        genList :: (Num a, Ord a) => SymTable a -> [a] -> [Command a] -> [Either String ([a], SymTable a, [a])]
        genList _ _ [] = []
        genList ml il (x:xc) = case interpretCommand ml il x of
            (Left err, _, _)   -> [Left err]
            (Right l, mem, l2) -> Right (l, mem, l2) : genList mem l2 xc

        processList :: (Num a, Ord a) => [Either String ([a], SymTable a, [a])] -> Either String ([a], SymTable a, [a])
        processList xs = case last xs of
            Left err           -> Left err
            Right (_, mem, l2) -> Right (concatOut xs, mem, l2)

        concatOut :: (Num a, Ord a) => [Either String ([a], SymTable a, [a])] -> [a]
        concatOut []                 = []
        concatOut (Right (l,_,_):xs) = l++concatOut xs
--Cond
interpretCommand m i (Cond bool com1 com2) = case eval (numValue m) bool of
    Left err -> (Left err, m, i)
    Right 1  -> interpretCommand m i com1
    _        -> interpretCommand m i com2
--Loop
interpretCommand m i (Loop bool com) = case eval (numValue m) bool of
    Left err -> (Left err, m, i)
    Right 1  -> interpretCommand m i (Seq [com, Loop bool com])
    _        -> (Right [], m, i)

interpretProgram:: (Num a,Ord a) => [a] -> Command a -> Either String [a]
interpretProgram i comm = case interpretCommand EmptyTable i comm of
    (Left err, _, _)  -> Left err
    (Right out, _, _) -> Right out


---------------------------------------------------------------------------------
----------------------------------PROGRAM TESTING--------------------------------
---------------------------------------------------------------------------------


printRes :: (Show a) => Either String [a] -> String
printRes (Left err) = "ERROR: " ++ err
printRes (Right ls) = show ls

randomIntList :: StdGen -> [Int]
randomIntList = randomRs (-1000,1000)

randomRealList :: StdGen -> [Double]
randomRealList = randomRs (-1000.0,1000.0)

runTests :: (Num a, Ord a) => Command a -> Int -> Int -> StdGen -> [Either String [a]]
runTests _ _ 0 _ = []
runTests com nex mode gen = case mode of
    0 -> [interpretProgram (randomIntList gen) com] ++ runTests com (nex-1) mode gen
    1 -> [interpretProgram (randomRealList gen) com] ++ runTests com (nex-1) mode gen

printTests :: (Num a, Ord a, Show a) => Int -> [Either String [a]] -> String
printTests _ [] = ""
printTests n (x:xs) = "Resultat "++show (n-(length xs))++": "++printRes x++"\n"++printTests n xs





main :: IO ()
main = do
    seed <- newStdGen
    fitxer <- openFile "programhs.txt" ReadMode
    programa <- hGetLine fitxer
    let prog :: (Read a, Num a) => Command a
        prog = read programa
    putStrLn "0 - Nombres enters  1-Nombres reals"
    intreal <- getLine
    putStrLn "Tipus d'execucio:"
    putStrLn "\t0-Execució manual."
    putStrLn "\t1-Test únic."
    putStrLn "\t2-Test múltiple."
    option <- getLine
    case option of
        "0" -> do
            putStrLn "Entrada:"
            inp <- getLine
            putStr "Resultat: "
            case intreal of
                "0" -> putStrLn $ printRes (interpretProgram (read inp++randomIntList seed) prog)
                "1" -> putStrLn $ printRes (interpretProgram (read inp++randomRealList seed) prog)
        "1" -> do
            putStr "Resultat: "
            case intreal of
                "0" -> putStrLn $ printRes (interpretProgram (randomIntList seed) prog)
                "1" -> putStrLn $ printRes (interpretProgram (randomRealList seed) prog)
        "2" -> do
            putStrLn "Nombre de tests:"
            tests <- getLine
            let tests = read tests
            putStr printTests tests $ runTests prog tests (read intreal) seed
    putStr "Programa indentat:\n"
    print prog
    putStr "Resultat de l'execucio:\n"
    putStr $ printRes (interpretProgram (randomIntList seed) prog)
    hClose fitxer
    return ()



