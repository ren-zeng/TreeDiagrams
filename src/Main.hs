module Main (main) where

import Data.Aeson (decode, decodeFileStrict, encodeFile)

import Control.Monad
import Data.Tree (Tree (..))
import Diagrams
import Diagrams.Backend.SVG (renderSVG)
import System.Directory
import Visualization.Text (drawText)
import Visualization.Tree (treeDiagram)

main :: IO ()
main = do
    treeFolderPath <- putStrLn "Enter tree folder path (withSlash)" >> getLine
    savePath <- putStrLn "Enter output folder path (withSlash)" >> getLine
    jsonNames <- listDirectory treeFolderPath
    forM_ jsonNames $ \s ->
        processSingleTree (treeFolderPath ++ s) (savePath ++ generateSVGFileName s)

processSingleTree :: FilePath -> FilePath -> IO ()
processSingleTree inPath outPath = do
    mTree <- decodeFileStrict inPath
    let diagram = case mTree of
            Nothing -> error "JSON format incorrect"
            Just t -> treeDiagram drawText t
    renderSVG outPath (mkWidth 1000) diagram

generateSVGFileName :: String -> String
generateSVGFileName xs = takeWhile (/= '.') xs ++ ".svg"
