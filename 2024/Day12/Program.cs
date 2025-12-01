using System.Drawing;
using System.Runtime.CompilerServices;

var numbers = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt").ToArray();
var map = new Dictionary<(int x, int y), char>();
var regions = new List<Region>();

for (var y = 0; y < numbers.Length; y++)
{
    for (var x = 0; x < numbers[y].Length; x++)
    {
        map[(x, y)] = numbers[y][x];
    }
}

var visited = new HashSet<(int x, int y)>();
foreach (var (position, name) in map)
{
    if (visited.Contains(position)) continue;

    var directions = new (int dx, int dy)[] { (0, 1), (1, 0), (0, -1), (-1, 0) };
    var queue = new Queue<(int x, int y)>();
    queue.Enqueue(position);

    var perimeter = 0;
    var area = 0;
    var positions = new HashSet<(int x, int y)>();
    var fences = new HashSet<Fence>();
    while (queue.Count > 0)
    {
        var current = queue.Dequeue();
        if (visited.Contains(current)) continue;
        visited.Add(current);
        positions.Add(current);
        area++;
        foreach (var d in directions)
        {
            var next = (current.x + d.dx, current.y + d.dy);
            if (map.TryGetValue(next, out var nextName) && nextName == name)
            {
                queue.Enqueue(next);
            }
            else
            {
                fences.Add(new Fence(name, current, d switch
                {
                    (0, 1) => Side.Bottom,
                    (1, 0) => Side.Right,
                    (0, -1) => Side.Top,
                    (-1, 0) => Side.Left,
                    _ => throw new InvalidOperationException()
                }));
                perimeter++;
            }
        }
    }

    var fenceQueue = new Queue<Fence>();
    var visitedFences = new HashSet<Fence>();
    var distinctFences = new HashSet<Fence>();
    foreach (var fence in fences)
    {
        if (visitedFences.Contains(fence)) continue;

        distinctFences.Add(fence); // add the first fence, and then find all connected fences
        fenceQueue.Enqueue(fence);
        while (fenceQueue.Count > 0)
        {
            var current = fenceQueue.Dequeue();
            if (visitedFences.Contains(current)) continue;

            visitedFences.Add(current);
            foreach (var d in directions)
            {
                var next = current with { Position = (current.Position.x + d.dx, current.Position.y + d.dy) };
                if (fences.Contains(next))
                {
                    fenceQueue.Enqueue(next);
                }
            }
        }
    }

    regions.Add(new Region(name, positions, area, perimeter, BulkPerimeter: distinctFences.Count));
}

foreach (var region in regions)
{
    Console.WriteLine(region);
}

Console.WriteLine(regions.Sum(r => r.Perimeter * r.Area));
Console.WriteLine(regions.Sum(r => r.BulkPerimeter * r.Area));

return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

internal record Region(char Name, HashSet<(int x, int y)> Positions, int Area, int Perimeter, int BulkPerimeter);

enum Side
{
    Top,
    Right,
    Bottom,
    Left
}

internal record Fence(char Name, (int x, int y) Position, Side Side);