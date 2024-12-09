using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;

var lines = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt")
    .Select(l => Regex.Match(l, @"(\d+): (.*)")).ToArray();
var part1Total = 0L;
var part2Total = 0L;
foreach (var line in lines)
{
    var target = long.Parse(line.Groups[1].Value);
    var values = line.Groups[2].Value.Split(" ").Select(long.Parse).ToArray();

    if (Part1(target, values[0], values[1..]))
    {
        part1Total += target;
    }
    if (Part2(target, values[0], values[1..]))
    {
        part2Total += target;
    }
}

Console.WriteLine(part1Total);
Console.WriteLine(part2Total);
return;

bool Part1(long target, long current, long[] values)
{
    if (values.Length == 0)
    {
        return target == current;
    }

    return Part1(target, current + values[0], values[1..]) ||
           Part1(target, current * values[0], values[1..]);
}

bool Part2(long target, long current, long[] values)
{
    if (values.Length == 0)
    {
        return target == current;
    }

    return Part2(target, current + values[0], values[1..]) ||
           Part2(target, current * values[0], values[1..]) ||
           Part2(target, long.Parse(current.ToString() + values[0].ToString()), values[1..]);
}

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;