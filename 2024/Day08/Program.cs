using System.Runtime.CompilerServices;

var input = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt");
var antennas = new Dictionary<(int x, int y), char>();
for (var y = 0; y < input.Length; y++)
{
    for (var x = 0; x < input[y].Length; x++)
    {
        var c = input[y][x];
        if (c != '.')
        {
            antennas[(x, y)] = c;
        }
    }
    Console.WriteLine(input[y]);
}

var maxY = input.Length;
var maxX = input[0].Length;
var part1Antinodes = new HashSet<(int x, int y)>();
var part2Antinodes = new HashSet<(int x, int y)>();
foreach (var (a1, c1) in antennas)
{
    foreach (var (a2, c2) in antennas)
    {
        if (c1 == c2 && a1 != a2)
        {
            var dx = a2.x - a1.x;
            var dy = a2.y - a1.y;
            (int x, int y) antinode = (a1.x + 2 * dx, a1.y + 2 * dy);
            if (antinode.x >= 0 && antinode.x < maxX && antinode.y >= 0 && antinode.y < maxY)
            {
                part1Antinodes.Add(antinode);
            }

            var n = 1;
            while (true)
            {
                antinode = (a1.x + n * dx, a1.y + n * dy);
                if (antinode.x >= 0 && antinode.x < maxX && antinode.y >= 0 && antinode.y < maxY)
                {
                    if (n == 2)
                    {
                        part1Antinodes.Add(antinode);
                    }

                    part2Antinodes.Add(antinode);
                    n++;
                }
                else
                {
                    break;
                }
            }
        }
    }
}

Console.WriteLine(part1Antinodes.Count);
Console.WriteLine(part2Antinodes.Count);
return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;