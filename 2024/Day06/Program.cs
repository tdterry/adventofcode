using System.Runtime.CompilerServices;
using Point = (int X, int Y);

// using State = ((int X, int Y) Position, char Direction);

{
    var map = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt").Select(l => l.ToCharArray()).ToArray();
    HashSet<State> visited = [];

    Point start = (0, 0);
    for (var y = 0; y < map.Length; y++)
    {
        for (var x = 0; x < map[y].Length; x++)
        {
            if (map[y][x] == '^')
            {
                start = (x, y);
                map[y][x] = '.';
            }
        }
    }

    var (pos, dir) = (start, '^');
    RunSim(map, visited, new State(start, dir)); // run once to find all visited positions
    // PrintMap(map, visited);
    Console.WriteLine("visited " + visited.Select(v => v.Position).Distinct().Count() + " positions");

    // any position that was visited could potentially have an obstacle added
    var obstacles = visited.Select(s => s.Position).Distinct().Sum(v =>
    {
        map[v.Y][v.X] = '#';
        var result = RunSim(map, [], new State(start, dir));
        map[v.Y][v.X] = '.';
        return result;
    });

    Console.WriteLine(obstacles + " obstacle positions");
}
return;

Point NextPosition(State s) => s.Direction switch
{
    '^' => (s.Position.X, s.Position.Y - 1),
    'v' => (s.Position.X, s.Position.Y + 1),
    '<' => (s.Position.X - 1, s.Position.Y),
    '>' => (s.Position.X + 1, s.Position.Y),
    _ => throw new Exception("Invalid direction")
};

char NextDirection(State s) => s.Direction switch
{
    '^' => '>',
    'v' => '<',
    '<' => '^',
    '>' => 'v',
    _ => throw new Exception("Invalid direction")
};

void PrintMap(char[][] map, HashSet<State> visited)
{
    for (var y = 0; y < map.Length; y++)
    {
        for (var x = 0; x < map[y].Length; x++)
        {
            if (visited.Select(v => v.Position).Any(p => p == (x, y)))
            {
                Console.Write("X");
            }
            else
            {
                Console.Write(map[y][x]);
            }
        }

        Console.WriteLine();
    }
}

State? NextState(char[][] map, State state)
{
    var next = NextPosition(state);
    if (next.X < 0 || next.X >= map[0].Length || next.Y < 0 || next.Y >= map.Length)
    {
        // escaped the map
        return null;
    }

    return map[next.Y][next.X] switch
    {
        '#' => state with { Direction = NextDirection(state) },
        _ => state with { Position = next }
    };
}

int RunSim(char[][] map, HashSet<State> visited, State? state)
{
    while (state != null)
    {
        if (!visited.Add(state))
        {
            return 1;
        } // loop detected

        state = NextState(map, state);
    }

    return 0;
}

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;
internal record State((int X, int Y) Position, char Direction);
