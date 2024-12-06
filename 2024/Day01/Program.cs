// See https://aka.ms/new-console-template for more information

using System.Text.RegularExpressions;
using System.Runtime.CompilerServices;

var lines = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt");
var numbers = lines.Select(ParseLine).ToArray();

var firstNumbers = numbers.Select(x => x.Item1).ToList();
var secondNumbers = numbers.Select(x => x.Item2).ToList();

firstNumbers.Sort();
secondNumbers.Sort();

var totalDistance = firstNumbers.Zip(secondNumbers, (first, second) => Math.Abs(second - first)).ToArray().Sum();
Console.WriteLine(totalDistance);

var totalSimilarity = 0;
foreach (var i in firstNumbers)
{
    var count = secondNumbers.Count(j => i == j);
    totalSimilarity += i * count;
}

Console.WriteLine(totalSimilarity);
return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

(int fst, int snd) ParseLine(string line)
{
    var groups = Regex.Match(line, @"(\d+)\s+(\d+)").Groups;
    return (int.Parse(groups[1].Value), int.Parse(groups[2].Value)); 
}