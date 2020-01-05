module Main where

import Lib

import System.Environment
import System.IO
--import Control.Alternative
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List
import Data.Either
import System.Exit
import Text.Printf
import Debug.Trace

replace a b = map $ maybe b id . mfilter (/= a) . Just

readAllFile :: [String] -> String -> IO String
readAllFile [] result = return (result)
readAllFile (x:xs) allFile = do
    result <- readFile x
    readAllFile xs $ allFile ++ result

-- list -> '(' expression* ')'
-- expression -> atom | list
-- atom -> number | name | string | operator

checkInterpreter :: String -> [String] -> Bool
checkInterpreter op args = elem op args

cleanArgs :: [String] -> [String]
cleanArgs args = do
    if elem "-i" args
        then
            do
                let new_args = delete "-i" args
                cleanArgs new_args
        else
            args

main :: IO ()
main = do
    args <- getArgs
    let needInterpreter = checkInterpreter "-i" args
    let new_args = cleanArgs args
    if needInterpreter
      then
        do
            x <- readAllFile new_args ""
            let allFile =  x
            let truc = removeTabandSpaces (removeCarriageReturn (replace '\t' ' ' allFile))
            -- print truc
            readEvalPrintLoop allFile
      else
        return()

readEvalPrintLoop :: String -> IO ()
readEvalPrintLoop str
    | str == "" = do
        putStr "> "
        hFlush stdout
        eofcheck <- isEOF
        if eofcheck
            then
                return()
            else
                do
                    -- printf "%.2f\n" $ runParser "1+1"
                    -- print str
                    line <- getLine
                    let result = str ++ line
                    -- printf "catched line = %s\n" line
                    readEvalPrintLoop result
    | otherwise = do
        printf "%.2f\n" $ runParser str
        putStr "> "
        hFlush stdout
        eofcheck <- isEOF
        if eofcheck
            then
                return()
            else
                do
                    -- printf "%.2f\n" $ runParser "1+1"
                    -- print str
                    line <- getLine
                    let result = str ++ line
                    -- printf "catched line = %s\n" line
                    readEvalPrintLoop result
       

removeTabandSpaces :: String -> String
removeTabandSpaces str = do
                        let clean = unwords . words
                        clean str

removeCarriageReturn :: String -> String
removeCarriageReturn str = [ x | x <- str, not (x `elem` "\n") ]

----------------------------------------------------------------------------------------------

data Expr = Add [Expr] -- suite de numlist dans la numlist on a des nums
    | Sub [Expr] -- normalement une Numlist
    | Mul [Expr] -- normalement une Numlist
    | Div [Expr] -- normalement une Numlist
    | Pow Expr Expr -- normalement une Numlist
    | Num Double -- normalement des doubles

-- data Expr = Add Expr Expr
--     | Sub Expr Expr
--     | Mul Expr Expr
--     | Div Expr Expr
--     | Pow Expr Expr
--     | Num Double


calcul :: [Expr] -> Char -> Double -> Bool -> Double
calcul [] _ result _ = result
calcul (x:xs) opera _ False = calcul xs opera (eval x) True
calcul (x:xs) opera result ver
    | opera == '+' = calcul xs opera (pAdd result (eval x)) True
    | opera == '-' = calcul xs opera (pSub result (eval x)) True
    | opera == '*' = calcul xs opera (pMult result (eval x)) True
    | opera == '/' = calcul xs opera (pDiv result (eval x)) True
    | otherwise = calcul xs opera (eval x) True

pAdd :: Double -> Double -> Double
pAdd d1 d2 = d1 + d2

pSub :: Double -> Double -> Double
pSub d1 d2 = d1 - d2

pMult :: Double -> Double -> Double
pMult d1 d2 = d1 * d2

pDiv :: Double -> Double -> Double
pDiv d1 d2 = d1 / d2

powerNumber :: Double -> Double -> Double -> Double
powerNumber num mult pow
    | pow == 0 = 1
    | pow == 1 = num
    | pow > 1 = powerNumber (num*mult) (mult) (pow-1)
    | pow < 0 = 1 / (powerNumber (num*mult) (mult) (-pow))

eval :: Expr -> Double
eval exp = case (exp) of
    (Add d1) -> calcul d1 '+' 0 False -- + [(eval num1), (eval num2), (eval num3)]
    (Sub d1) -> calcul d1 '-' 0 False
    (Mul d1) -> calcul d1 '*' 0 False
    (Div d1) -> calcul d1 '/' 0 False
    (Pow d1 d2) -> powerNumber (eval d1) (eval d1) (eval d2)
    (Num d1) -> d1

newtype Parser a = Parser {runEvalExpr :: String -> Either String (String, a)}



-- pFactor :: Parser Expr
-- pFactor = do
    --     pNum
    --     <|>
--     pChar '(' *> pExpr <* pChar ')'

-- pPow :: Parser Expr
-- pPow = do
    --     lv <- pFactor
    --     op <- pChar '^'
    --     rv <- pPow
    --     case (op) of
        --         '^' -> return (Pow lv rv)
        --     <|>
        --     pFactor
        
-- pTerm :: Parser Expr
-- pTerm = do
    --     lv <- pPow
    --     op <- pChar '*' <|> pChar '/'
    --     rv <- pTerm
    --     case (op) of
        --         '*' -> return (Mul lv rv)
        --         '/' -> return (Div lv rv)
        --     <|>
        --     pPow
                
-- pExpr :: Parser Expr
-- pExpr = do
    --     lv <- pTerm
    --     op <- pChar '+' <|> pChar '-'
    --     rv <- pExpr
    --     case (op) of
        --         '+' -> return (Add lv rv)
        --         '-' -> return (Sub lv rv)
        --     <|>
        --     pTerm
                        
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy veriChar = Parser fun
    where
        fun [] = Left "Error empty string"
        fun (x:xs)
            | veriChar x = Right (xs, x)
            | otherwise = Left "Error bad caractere"

pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

pDigit :: Parser Char
pDigit = pSatisfy (\c -> elem c "1234567890.")

pDigits :: Parser String
pDigits = some pDigit

pDouble :: Parser Double
pDouble = fmap read pDigits

pNum :: Parser Expr
pNum = fmap Num pDouble

pName :: Parser Expr
pName = undefined

pString :: Parser Expr
pString = undefined

pOneOf :: String -> Parser Char
pOneOf str = pSatisfy (\c -> elem c str)

pSpace :: Parser String
pSpace = many $ pOneOf " "

pNumList :: Char -> Parser Expr
pNumList op = do
    numList <- some (pNum <* pSpace) -- pOneOf sert a parser les espaces
    case op of
        '+' -> return(Add numList)
        '-' -> return(Sub numList)
        '*' -> return(Mul numList)
        '/' -> return(Div numList)

pOperator :: Parser Expr
pOperator = do
    op <- pChar '+' <|> pChar '-' <|> pChar '*' <|> pChar '/'
    pSpace
    numList <- pNumList op 
    return(numList)

pAtom :: Parser Expr
pAtom = do
    pOperator
    <|> pExpr
    <|> pNum
    <|> pName
    <|> pString

pExpr :: Parser Expr
pExpr = do
    pAtom <|> pList

pList :: Parser Expr
pList = do
    pChar '(' *> pExpr <* pChar ')'

-- pExpr :: Parser Expr
-- pExpr = do
--     lv <- pTerm
--     op <- pChar '+' <|> pChar '-'
--     rv <- pExpr
--     case (op) of
--         '+' -> return (Add lv rv)
--         '-' -> return (Sub lv rv)
--     <|>
--     pTerm

evalExpr :: String -> Double
evalExpr str = eval fun
    where
        fun = case (runEvalExpr (pList) str) of -- mettre pList
            Right (str, a) -> a

runParser :: String -> Double
runParser str = evalExpr str

pBind :: Parser a -> (a -> Parser b) -> Parser b
pBind pa apb = Parser fun
        where
            fun str = case (runEvalExpr pa str) of
                Left msg -> Left msg
                Right (str', a) -> runEvalExpr (apb a) str'

pBack :: a -> Parser a
pBack a = Parser fun
        where
            fun str = Right (str, a)

pSequential :: Parser(a -> b) -> Parser a -> Parser b
pSequential pab pa = Parser fun
        where
            fun str = case (runEvalExpr pab str) of
                Left msg -> Left msg
                Right (str', ab) -> case (runEvalExpr pa str') of
                    Left msg -> Left msg
                    Right (str'', a) -> Right (str'', ab a)

pFmap :: (a -> b) -> Parser a -> Parser b
pFmap fab pa = Parser fun
    where
        fun str = case (runEvalExpr pa str) of
            Left msg -> Left msg
            Right (str', a) -> Right (str',(fab a))

pEmpty :: Parser a
pEmpty = Parser fun
    where
        fun str = Left "Data empty"

pOr :: Parser a -> Parser a -> Parser a
pOr pa1 pa2 = Parser fun
    where
        fun str = case (runEvalExpr pa1 str) of
            Left msg -> case (runEvalExpr pa2 str) of
                Left msg -> Left msg
                Right (str'', a') -> Right (str'', a')
            Right (str', a) -> Right (str', a)

pSequenceActionsSecond :: Parser a -> Parser b -> Parser a
pSequenceActionsSecond pa pb = Parser fun
                where
                    fun str = case (runEvalExpr pa str) of
                       Left msg -> Left msg
                       Right (str', a) -> case (runEvalExpr pb str') of
                            Left msg -> Left msg
                            Right (str'', b) -> Right (str'', a)
 

instance Functor Parser where
    fmap = pFmap

instance Applicative Parser where
    pure = return
    (<*>) = pSequential
    (<*) = pSequenceActionsSecond

instance Alternative Parser where
    empty = pEmpty
    (<|>) = pOr

instance Monad Parser where
    return = pBack
    (>>=) = pBind
