import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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

