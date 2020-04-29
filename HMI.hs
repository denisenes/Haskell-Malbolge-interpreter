import qualified Data.List as L
import qualified Data.Vector as V
import Data.Char
import Control.Monad.State.Strict
import Data.Bits
import Control.DeepSeq
import System.Environment
import Data.Maybe


--Строки шифрования
xlat1 = "+b(29e*j1VMEKLyC})8&m#~W>qxdRp0wkrUo[D7,XTcA\"lI.v%{gJh4G\\-=O@5`_3i<?Z';FNQuY]szf$!BS/|t:Pn6^Ha"
xlat2 = "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1CB6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@"

memorySize = 59049

crazyTable = [[1, 0, 0],
              [1, 0, 2],
              [2, 2, 1]]

type Register = Int
type Flag = Int
type StdOut = String
type StdIn = String


--получаем строку из инпута, удаляем пробелы, переносы строки и проверяем корректность введенных символов, а также размер программы
parser:: String -> String
parser str = reverse $! (helper str [] $!! 0) where
   helper:: String -> String -> Int -> String
   helper [] acc _ = acc
   helper (a:str) acc i | elem (ord a) spaces == True = helper str acc $! i
                        | elem (com a i) correctSymbols == False = error "Program is not correct"
                        | i > 59049 = error "Program is too BIG"
                        | otherwise = helper str (a:acc) $! (i + 1)
   com c d = (!!) xlat1 $! (((ord c) - 33 + d) `mod` 94)
   spaces = [10, 13, 32]
   correctSymbols = ['i', 'j', 'o', 'p', 'v', '*', '/', '<']

--заполняем память виртуальной машины. Сначала кладем в нее программу, затем 
--до конца памяти производим операцию crazy для всех пустых ячеек
fillMem:: [Char] -> [Int]
fillMem d = (++) (map (\x -> ord x) $! d) $! (crazyFill (ord $ last $! d) (ord $ last $ init $! d) (memorySize - (length $! d)) $! []) where
   crazyFill _ _ 0 acc = reverse $! acc
   crazyFill a1 a2 i xss = crazyFill (crazyRes a1 $! a2) a1 (i-1) $!! ((crazyRes a1 $! a2):xss)
   crazyRes a b = crazy a $! b

--Функция crazy
crazy:: Int -> Int -> Int
crazy b a = helper a b 0 $! 0 where
   helper _ _ res 10 = res
   helper x1 x2 res i = helper (x1 `div` 3) (x2 `div` 3) (res + (tritCRZ (x1 `mod` 3) $! (x2 `mod` 3)) * 3^i) $! (i + 1)
   --Выполняем операцию crazy для одного трита по таблице
   tritCRZ:: Int -> Int -> Int
   tritCRZ t1 t2 = head $ snd $ splitAt t2 $! (line $! t1)
   line x = head $ snd $ splitAt x $! crazyTable

--Выполняем программу
runProgram:: Flag -> State (V.Vector Int, Register, Register, Register, StdOut, StdIn) String
runProgram 1 = do
                  (_, _, _, _, str, _) <- get
                  return $! (reverse $! str)
runProgram k = do
            (mem, c, d, a, str, stdin) <- get
            let symbol = getChar mem $! c
            let acase = aFoo mem $! d
            case symbol of
               'j' -> put $! (mem, c, ((V.!) mem $! d), a, str, stdin)
               'i' -> put $! (mem, ((V.!) mem $! d), d, a, str, stdin)
               '*' -> put $! (putInMem mem d $! acase, c, d, acase, str, stdin)
               'p' -> put $! (putInMem mem d $! (crazy a $!! ((V.!) mem $! d)), c, d, crazy a $! ((V.!) mem $! d), str, stdin)
               '<' -> put $! (mem, c, d, a, ((chr $! ( 255 .&. a)):str), stdin)
               '/' -> put $! (mem, c, d, (getSymbol $! stdin), (str), myTail $! stdin)
               'v' -> put $! (mem, c, d, -1, str, stdin)
               'o' -> put $! (mem, c, d, a, str, stdin)
               _ -> put $! (mem, c, d, a, str, stdin)
            (mem, c, d, a, str, stdin) <- get
            put $!! (count mem $! c, mod (c+1) $! 59049, mod (d+1) $! 59049, a, str, stdin)
            (_, _, _, p, _, _) <- get
            if (p == -1) then runProgram $! 1 else runProgram $! 0

         where --scary place X_X...
            count mem1 c1 = V.force ((V.++) (V.force $ (V.++) (V.take c1 $! mem1) $! (V.singleton (ord $! ((!!) xlat2 $! (((V.!) mem1 $! c1) - 33))))) $! (V.drop (c1 + 1) $! mem1))
            putInMem memx x exp = V.force ((V.++) (V.take x $! memx) $! ((V.++) (V.singleton exp) $! (V.drop (x + 1) $! memx)))
            getChar mem1 c1 = (!!) xlat1 $! (mod (((V.!) mem1 $! c1) - 33 + c1) $! 94)
            aFoo mem1 d1 = (div ((V.!) mem1 $! d1) $! 3) + ((mod ((V.!) mem1 $! d1) $! 3) * 19683)
            --input
            getSymbol [] = 59048
            getSymbol (a:str) = ord $! a
            --myTail
            myTail [] = []
            myTail (_:xss) = xss
            getMem m1 = m1
               
               
                     
main::IO()
main = do
   putStrLn $! "Haskell Malbolge Interpreter v.0.9"
   args <- getArgs
   prog <- readFile $! (head $! args)
   input <- readFile $! "input.txt"
   putStr $! "Input: "
   print $! input
   putStrLn $! "=================================="
   let parsedData = parser $! prog
   let memory = fillMem $! parsedData
   let startState = (force $ V.fromList memory, 0, 0, 0, "", input)
   let stdout = evalState (runProgram $!! 0) startState
   putStrLn $! "Output:"
   print $! stdout
