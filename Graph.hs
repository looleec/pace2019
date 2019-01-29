module Graph(
    Graph,
    getNVertices,
    getNEdges,
    getEdges,
    checkG,
    emptyG,
    deleteVertice,
    mostNeighbours,
    neighbours
) where
  
import qualified Data.Ord                      as O
import qualified Data.Foldable                 as F
import qualified Data.List                     as L
import qualified Data.Maybe                    as M

-- | 'Graph' type
data Graph = Graph { 
    getNVertices    :: Int          -- the number of vertices
    , getNEdges     :: Int          -- the number of edges
    , getEdges      :: [(Int, Int)] -- the edges 
} deriving (Show)

-- |'emptyG' returns an empty graph
emptyG :: Graph
emptyG = Graph { getNVertices = 0, getNEdges = 0, getEdges = [] }

-- |'checkG' 'g' returns 'True' if the graph 'g' is valid.
checkG :: Graph -> Bool
checkG g = checkNVertices g && checkNEdges g && checkDuplicate g && checkEdges g
  where
    checkNVertices g = getNVertices g == F.maximum (L.map (uncurry max) $ getEdges g)
    checkNEdges    g = getNEdges g == L.length (getEdges g)
    checkDuplicate g = L.length (getEdges g) == L.length (L.nub $ getEdges g)
    checkEdges       = F.all (uncurry (/=)) . getEdges

-- | 'deleteVertice' returns a new graph minus a vertice
deleteVertice :: Int -> Graph -> Graph
deleteVertice v g = g { getNVertices = getNVertices g - 1, getNEdges = L.length edges, getEdges = edges}
    where
        edges = filter (\(a,b) -> (not( a == v || b == v))) $ getEdges g

-- | 'mostNeighbours' returns the one who has the most neighbours
mostNeighbours :: Graph -> Int
mostNeighbours g = L.head . L.head . L.sortBy (\xs ys -> O.compare (L.length xs) (L.length ys)) $ L.groupBy (\a b -> a == b) flat
    where
        flat = [x | (a,b) <- (getEdges g), x <- [a,b]]


-- | 'neighbours' returns a list of the neighbours 
neighbours :: Int -> Graph -> [Int]
neighbours v g = [if a == v then b else a | (a, b) <- (getEdges g)]





