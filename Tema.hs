import Data.List (group, sort, sortBy)
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Char (toLower)
import System.IO                      
import System.Directory
import Data.List

import Text.Show
import Control.Applicative

import System.Console.ANSI -- ANSI
import Control.Monad -- Pentru let
import System.Process -- Pentru clear


-- Main
main:: IO ()
main = do

   ----- Citirea numelui pentru fisierul dorit -----
   line <- promptLine "Introduceti numele fisierului dorit: (Default: textDefault.txt)"
   ----- Verificarea fisierului dpdv corectitudine + existenta -----
   fileName <- readFileName line 
   
   ----- Afisarea numelui fisierului selectat -----
   putStrLn (fileName ++ " gata de utilizare!")

   ----- Bubcla Main -----
   mainLoop fileName
   

----- Functia pentru Bucla Main -----
mainLoop::String -> IO()
mainLoop fileName = do
   
   let loop = do {

      ----- Afisarea titlului Temei -----
      showTitle;

      ----- Selectezi optiunea dorita -----
      option <- showMenu;

      ----- Tratarea erorilor in cazul in care optiunea este invalida -----
      executeMenuCommand (option, fileName);

      input <- promptLine "\n\nContinuati alegerea optiunilor apasand Enter";

      ----- Stergem ecranul anterior -----
      system "cls";

      ----- Bucla repetitiva -----
      loop;
   }

   loop -- Incepe iteratia 


----- Functie pentru citirea si validarea unui fisier -----
readFileName::String -> IO String
readFileName line = do 

   ----- Validare pentru numele fisierului -----
   --- boolIO - variabila de tip bool - folosita pentru verificare
   boolIO <- doesFileExist line
   if not boolIO
      --- textDefault.txt - fisier prestabilit pentru utilizare 
      then return "textDefault.txt" 
      else return line


----- Functie principala pentru Meniu -----
showMenu::IO String
showMenu = do

   setSGR [SetColor Foreground Dull Yellow];

   putStrLn "==========================================================="
   putStrLn "| Selectati optiunea dorita:                              |"
   putStrLn "| 1) Compuneti un text                                    |"
   putStrLn "| 2) Afisati textul dorit                                 |"
   putStrLn "| 3) Afisati tabelar si lexicografic cuvintele din fisier |"
   putStrLn "| 4) Stergeti textul                                      |"
   putStrLn "==========================================================="

   setSGR [SetColor Foreground Dull Green];
   putStrLn "\n"; 

   option <- promptLine "Selectati optiunea dorita: "
   putStrLn "\n";
   return (option)


----- Actiune referitoare la meniu -----
-- Utilizare garda pentru testarea daca proprietatea valorii e adevarata sau falsa -----
executeMenuCommand::(String, String) -> IO()
executeMenuCommand (option, fileName) 
   | option == "1" = appendTextForFile fileName       
   | option == "2" = showTextInFile fileName          
   | option == "3" = showFreqInFile fileName          
   | option == "4" = clearTextFile fileName           
   | option == option = putStrLn "Incorect! Introduceti o optiune valida!" -- <-- Optiune incorecta   

 
----- Simulare IO a promptline-ului din haskell : Scriere + citire Simultan -----
----- https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions de vazut OfficialHaskellDoc -----
promptLine::String -> IO String
promptLine prompt = do
  putStrLn prompt
  setSGR [SetColor Foreground Vivid Blue]
  getLine


----- Functie pentru introducerea textului in fisier -----
appendTextForFile:: String -> IO()
appendTextForFile fileName = do
   textulscris <- promptLine "Introduceti textul: "
   appendFile fileName (textulscris ++ " ")
   putStrLn "Textul a fost adaugat cu succes!"


-----Functie pentru afisarea textului din fisier -----
showTextInFile::String -> IO()
showTextInFile fileName = do
   
   putStrLn "Textul existent in fisier este: "
   textFromFile <- readFile fileName
   putStr textFromFile


----- Functie pentru afisarea frecventei cuvintelor existente in fisier -----
showFreqInFile::String -> IO()
showFreqInFile fileName = do
   
   setSGR [SetColor Foreground Dull White];
   ----- Citeste frecventa -----
   xs <-
            readFile fileName >>=
            \txt ->
            return
            (unlines $
            show <$>
            take
            100
         (sortBy
         (flip (comparing fst))
         ((length &&& head) <$> group (sort . words $ fmap toLower txt))))

   ----- Afiseaza frecventa -----
   putStrLn xs


----- Functie pentru stergerea textului din fisier -----
clearTextFile::String -> IO()
clearTextFile fileName = do

   writeFile fileName ""
   putStrLn "Textul a fost sters cu succes!"


----- ANSI ART pentru partea de inceput -----
showTitle = do {
   putStrLn "\n"; 
  setSGR [SetColor Foreground Dull Magenta];
  putStr "=================";
  putStr "|     HASKELL   |";
  putStr "=================";
  setSGR [SetColor Foreground Dull Blue];
  putStrLn "\n";
}