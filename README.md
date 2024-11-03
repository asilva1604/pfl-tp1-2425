# Practical Assignment 1 - PFL 2024/2025

## Group Members
- Alexandre Silva: 50%
    - Tasks:
- Eduardo Baltazar: 50%
    - Tasks:
## Shortest Path Function
### Implementation
The shortestPath function was implemented using Dijkstra's algorithm.

### Explanation
- Auxiliary Data Structures:
    - AdjList: The roadmap is converted to an adjacency list representation using the roadMapToAdjList function. This structure allows efficient access to neighboring cities and their distances.
    - Priority Queue: A priority queue is used to always expand the shortest known path first. This ensures that the algorithm efficiently finds the shortest path.
- Algorithm:
    - Dijkstra's Algorithm: This algorithm is used to find the shortest path from the starting city a to the destination city b. It works by iteratively expanding the shortest known path and updating the shortest paths to neighboring cities.
    - Initialization
      - The algorithm starts with the initial city a and sets the distance to itself as 0.
      - The distance to all other cities is set to infinity.
      - A priority queue is used to keep track of the cities to be explored, prioritized by their current known shortest distance.
    - Exploration
      - The city with the smallest known distance is extracted from the priority queue.
      - For each neighboring city, the algorithm calculates the distance through the current city.
      - If the calculated distance is smaller than the known distance, the shortest distance is updated, and the neighboring city is added to the priority queue.
    - Termination
      - The algorithm terminates when the destination city b is extracted from the priority queue, or when the priority queue is empty (indicating that there is no path from a to b).
    - Result
      - The shortest path from a to b is obtained by backtracking from b using the recorded shortest distances.


## Travel Sales Function
### Implementation
The travelSales function solves the Traveling Salesman Problem (TSP) to find the shortest path that visits all cities exactly once and returns to the starting city.

### Explanation
- Auxiliary Data Structures:
    - AdjList: Similar to the shortestPath function, the roadmap is converted to an adjacency list representation.
    - Distance Function: A function to calculate the distance between two cities, used by the TSP solver.
- Algorithm:
    - TSP Solver: The solveTSP function is used to solve the TSP. It finds the shortest path that visits all cities exactly once and returns to the starting city. The algorithm ensures that all cities are connected and uses the adjacency list and distance function to compute the optimal path.
  - Dynamic Programming with Bitmasking:
    - The algorithm uses dynamic programming to store the shortest paths for subsets of cities.
    - Bitmasking is used to represent subsets of cities. Each bit in the bitmask represents whether a city is included in the subset.
  - State Representation
    - The state is represented by two variables: pos (represents the current city) and mak (is a bitmask where each bit indicated whether a city has been visited)
  - Transition
    - From each state (pos, mask), the algorithm considers moving to an unvisited city i
    - The trnsition is only valid if city i has not been visited (check using bitmask)
  - Recurrence Relation
    - The algorithm uses a recurrence relation to compute the minimum distance for each state.
    - The distance for state (pos, mask) is the minimum of the distances obtained by moving to each unvisited city i and adding the distance from pos to i.
  - Base Case
    - The base case is when all cities have been visited (i.e., mask has all bits set). In this case, the distance is the distance from the current city back to the starting city.
  - Memoization
    - The algorithm uses a memoization table (dp) to store the minimum distances for each state (pos, mask).
    - This avoids recomputing the distances for the same state multiple times, improving efficiency.
  - Result Extraction
    - The final result is extracted from the memoization table, representing the shortest path that visits all cities and returns to the starting city.