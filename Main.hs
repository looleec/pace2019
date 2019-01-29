module Main where

import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified Data.Tuple         as T
import qualified System.Environment as Environment
import qualified System.IO          as IO
import qualified Graph              as G
import qualified Data.Maybe         as Maybe
import qualified Debug.Trace        as Debug


-- |'stringToInt' 's' converts the string 's' to integer
stringToInt :: String -> Int
stringToInt s = read s :: Int

-- Convenience function.
whenMaybe :: a -> Bool -> Maybe a
whenMaybe _ False = Nothing
whenMaybe a True  = Just a

-- |'parseG' 's' parses a graph given as a PACE 19 format list of strings.
-- The function returns 'Nothing' in case of parse error.
parseG :: [String] -> Maybe G.Graph
parseG = go G.emptyG . L.map L.words . L.filter (not . L.isPrefixOf "c")
  where
    go g []                    = whenMaybe g (G.checkG g)
    go g (["p","td",n,m] : ls) = go (g { G.getNVertices = stringToInt n, G.getNEdges = stringToInt m }) ls
    go g ([i,j]          : ls)
      | i < j                  = go (g { G.getEdges = (stringToInt i, stringToInt j) : G.getEdges g  }) ls
      | otherwise              = go (g { G.getEdges = (stringToInt j, stringToInt i) : G.getEdges g  }) ls
    go _ _                     = Nothing


-- |'coverVertice' checks the vertice covered
coverVertice :: G.Graph -> [Int]
coverVertice g = L.sort $ coverVertice' g []

coverVertice' :: G.Graph -> [Int] -> [Int]
coverVertice' g xs
    | G.getNEdges g == 0 = xs
    | otherwise = coverVertice' (G.deleteVertice elt g) (elt:xs)
        where
            elt = Debug.traceShowId $ G.mostNeighbours g


-- Parse and display a graph.
main :: IO ()
main = do
    args    <- Environment.getArgs
    content <- IO.readFile (L.head args)
    IO.putStr . show . Maybe.fromJust . parseG $ L.lines content
    IO.putStr "\n"
    IO.putStr . show . G.mostNeighbours . Maybe.fromJust . parseG $ L.lines content
    IO.putStr "\n"
    IO.putStr . show . coverVertice . Maybe.fromJust . parseG $ L.lines content
    IO.putStr "\n"






