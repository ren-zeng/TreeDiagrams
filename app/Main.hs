module Main (main) where

import Data.Aeson (decode, decodeFileStrict, encodeFile)

import Control.Applicative (Alternative (some))
import Control.Monad
import Core.ProofTree
import Data.Tree (Tree (..))
import Diagrams
import Diagrams.Backend.SVG (renderSVG)
import System.Directory
import Visualization.ProofTree (drawProofTree)
import Visualization.Text (drawText)
import Visualization.Tree (treeDiagram)

import Prettyprinter
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT, MaybeT))

main :: IO ()
main = do
    treeFolderPath <- displayUserInput "Enter tree folder path (withSlash)" 
    savePath <- displayUserInput "Enter output folder path (withSlash)"
    jsonNames <- listDirectory treeFolderPath
    forM_ jsonNames $ \s ->
        processSingleTree (treeFolderPath ++ s) (savePath ++ generateSVGFileName s)

displayUserInput :: String -> IO String
displayUserInput prompt = do 
    putStrLn prompt
    x <- getLine
    -- print $ indent 4 $ pretty x 
    return x
 

processSingleTree :: FilePath -> FilePath -> IO ()
processSingleTree inPath outPath = do
    maybeDiagram <- runMaybeT $ 
        msum 
            [ do
                t <- MaybeT $ decodeFileStrict @(Tree String) inPath
                return $ treeDiagram drawText t
            , do
                t <- MaybeT $ decodeFileStrict @(ProofTree String String) inPath
                return $ drawProofTree drawText drawText t
            ]
    case maybeDiagram of 
        Nothing -> error "JSON format not matching either tree or proofTree, correct json examples can be found at ``exampleTree.json'' or ``exampleProofTree.json''"
        Just diagram -> renderSVG outPath (mkWidth 1000) diagram

generateSVGFileName :: String -> String
generateSVGFileName xs = takeWhile (/= '.') xs ++ ".svg"
