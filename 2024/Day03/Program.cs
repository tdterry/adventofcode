// See https://aka.ms/new-console-template for more information

using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;

var content = System.IO.File.ReadAllText($"{GetThisFilePath()}/input.txt");

var part1 = Part1().Matches(content).Sum(ParseMul);
Console.WriteLine(part1);

var part2 = Part2().Matches(content).Aggregate(new State(true, 0),
    (state, match) => match.Value switch
    {
        "do()" => state with { Enabled = true }, 
        "don't()" => state with { Enabled = false },
        _ => state.Enabled ? state with { Sum = state.Sum + ParseMul(match) } : state
    }).Sum;
Console.WriteLine(part2);
return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

int ParseMul(Match match)
{
    var x = int.Parse(match.Groups[1].Value);
    var y = int.Parse(match.Groups[2].Value);
    return x * y;
}

partial class Program
{
    [GeneratedRegex(@"mul\((\d+),(\d+)\)")]
    private static partial Regex Part1();

    [GeneratedRegex(@"do\(\)|don't\(\)|mul\((\d+),(\d+)\)")]
    private static partial Regex Part2();
}

internal record State(bool Enabled, int Sum);