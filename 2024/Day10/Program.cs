using System.Runtime.CompilerServices;

var numbers = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt").ToArray();
var map = new Dictionary<(int x, int y), int>();
var trailHeads = new List<(int x, int y)>();
for (var y = 0; y < numbers.Length; y++)
{
    for (var x = 0; x < numbers[y].Length; x++)
    {
        if (numbers[y][x] == '.') continue;

        map[(x, y)] = numbers[y][x] - '0';
        if (map[(x, y)] == 0)
        {
            trailHeads.Add((x, y));
        }
    }
}

// Part 1
Console.WriteLine(trailHeads.Sum(trailHead => Solve(false, trailHead, [])));

// Part 2
Console.WriteLine(trailHeads.Sum(trailHead => Solve(true, trailHead, [])));

return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

int Solve(bool allUnique, (int x, int y) node, List<(int x, int y)> visited)
{
    visited.Add(node);
    if (map.TryGetValue(node, out var nodeValue) && nodeValue == 9)
    {
        return 1;
    }

    (int dx, int dy)[] directions = [(0, -1), (0, 1), (-1, 0), (1, 0)];
    return (from d in directions
            let next = (node.x + d.dx, node.y + d.dy)
            where map.TryGetValue(next, out var nextValue) && // next is in the map
                  nextValue == nodeValue + 1 && // next is a valid step
                  (allUnique || !visited.Contains(next)) // either we want all paths or next is not visited
            select Solve(allUnique, next, visited)
        ).Sum();
}