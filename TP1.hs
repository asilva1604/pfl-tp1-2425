import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City, [(City, Distance)])]

-- this function takes a RoadMap as an argument, and returns a kist of all the cities that are in it
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [cit | (c1, c2, _) <- roadmap, cit <- [c1,c2]] -- Extracts all cities from the roadmap, flattens them into a list, and removes duplicates using nub

-- this function takes a RoadMap and two City as arguments, and returns True if they are adjacent, as in, there is a road with them, and false otherwise
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap c1 c2 = 
    elem (c1, c2) [(c1, c2) | (c1, c2, _) <- roadmap] || -- if (c1,c2,_) exists in roadmap return true
    elem (c2, c1) [(c1, c2) | (c1, c2, _) <- roadmap] -- if (c2,c1,_) exists in roadmap return true

-- this function takes a RoadMap and two City as arguments, and returns a Just value of the distance between them if they are adjacent, and Nothing if they are not
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing -- base case
distance ((x, y, d):xs) c1 c2
    | (x == c1 && y == c2) || (x == c2 && y == c1) = Just d -- if they are connected, return Just distance
    | otherwise = distance xs c1 c2 -- else, use recursion

--this function returns a list of (adjCity, distance) where adjCity is an adjacent city to the City argument in the Roadmap argument and distance is the Distance between the two
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = [] --base case
adjacent ((c1, c2, d):es) city
    --if either city in an edge is equal to City arg, append (adjecentCity, distance)
    | (c1 == city) = (c2, d) : adjacent es city 
    | (c2 == city) = (c1, d) : adjacent es city
    --otherwise do not append
    | otherwise = adjacent es city

--this function returns the sum of distances between all consecutive cities in the Path argument; if between at least 2 consecutive cities in the Path argument there is no direct road (according to the RoadMap argument), this function returns Nothing
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [_] = Just 0   --empty path has 0 distance
pathDistance _ [] = Just 0   --path with a single city has 0 distance
pathDistance r (c1:c2:cs) =
    do
        d <- distance r c1 c2      --if distance r c1 c2 returns Nothing, do will also return nothing
        d_sum <- pathDistance r (c2:cs)    --same applies as line above
        return (d + d_sum)
    
    
--this function returns a list of all cities in RoadMap argument that are connected to the maximum number of roads
rome :: RoadMap -> [City]
rome r =
    let
        cities_roads = [(city, length (adjacent r city)) | city <- cities r]   --list of (city, number of roads connected to city) tuples
        max_roads = maximum [n_roads | (_, n_roads) <- cities_roads]    --maximum number of roads connected to a single city
        in [city | (city, n_roads) <- cities_roads, n_roads == max_roads]


------------------------------
--auxiliary functions

--this function returns an adjacency list made from the RoadMap argument
--toAdjMatrix :: RoadMap -> AdjList
--toAdjMatrix roadmap = [(city, roads) | city <- cities roadmap, roads <- adjacent roadmap city]

-----aux function for isStronglyConnected
dfs :: RoadMap -> [City] -> [City] -> [City]
dfs _ visited [] = visited
dfs r visited (c:cs)
    | elem c visited = dfs r visited cs
    | otherwise = dfs r (c:visited) ([c1 | (c1, _) <- adjacent r c] ++ cs)
------------------------------

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r =
    let
        allCities = cities r
        dfsVisited = dfs r [] [(head allCities)]
        in length dfsVisited == length allCities

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rmap a b = dijkstra (roadMapToAdjList rmap)  a b

roadMapToAdjList :: RoadMap -> AdjList
roadMapToAdjList rmap = Data.List.foldl' insertEdge [] rmap
  where
    insertEdge :: AdjList -> (City, City, Distance) -> AdjList
    insertEdge adjList (c1, c2, dist) = addNeighbor (addNeighbor adjList c1 (c2, dist)) c2 (c1, dist)

    addNeighbor :: AdjList -> City -> (City, Distance) -> AdjList
    addNeighbor [] city neighbor = [(city, [neighbor])]
    addNeighbor ((c, neighbors):rest) city neighbor
        | c == city = (c, neighbor:neighbors) : rest
        | otherwise = (c, neighbors) : addNeighbor rest city neighbor

neighbors :: AdjList -> City -> [(City, Distance)]
neighbors adjList city =
    let result = lookup city adjList
    in if result /= Nothing 
       then extract result 
       else []
  where
    extract (Just ns) = ns

dijkstra :: AdjList -> City -> City -> [Path]
dijkstra adjList start end = dijkstra' [(start, 0, [start])] [] where
    dijkstra' [] _ = []  -- No paths found
    dijkstra' ((current, d, path):queue) visited
        | current == end = [path] ++ extractOtherShortest end queue  -- Found shortest path(s)
        | current `elem` visited = dijkstra' queue visited  -- Skip if already visited
        | otherwise = dijkstra' newQueue (current : visited)
        where
            neighborsList = neighbors adjList current
            newQueue = foldl (\q (n, dist) -> addOrUpdateQueue n (d + dist) (path ++ [n]) q) queue neighborsList

addOrUpdateQueue :: City -> Distance -> Path -> [(City, Distance, Path)] -> [(City, Distance, Path)]
addOrUpdateQueue city newDist newPath queue =
    case Data.List.find (\(c, _, _) -> c == city) queue of
        Nothing -> (city, newDist, newPath) : queue
        Just (_, oldDist, oldPath) ->
            if newDist < oldDist then (city, newDist, newPath) : Data.List.delete (city, oldDist, oldPath) queue
            else if newDist == oldDist then (city, newDist, newPath) : queue  -- Keep all equal-distance paths
            else queue

-- Extract additional shortest paths to the end city
extractOtherShortest :: City -> [(City, Distance, Path)] -> [Path]
extractOtherShortest city queue = [path | (c, _, path) <- queue, c == city]

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

