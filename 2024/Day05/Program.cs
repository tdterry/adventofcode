// See https://aka.ms/new-console-template for more information

using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;

var input = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt");

var constraints = new HashSet<Constraint>();
var totalCorrect = 0;
var totalFixed = 0;

foreach (var line in input)
{
    var match = ConstraintPattern().Match(line);
    if (match.Length > 0)
    {
        constraints.Add(new Constraint(int.Parse(match.Groups[1].Value), int.Parse(match.Groups[2].Value)));
    }
    else if (line.Length > 0)
    {
        var pages = line.Split(",").Select(int.Parse).ToList();
        var pagesFixed = new List<int>(pages);
        
        // Sort according to the constraints
        pagesFixed.Sort((int a, int b) => constraints.Contains(new Constraint(a, b)) ? -1 : 1);

        // The original was correct if it is the same as the sorted version
        var valid = pages.SequenceEqual(pagesFixed);
        var middle = pagesFixed[pages.Count / 2];
        if (valid)
        {
            totalCorrect += middle;
        }
        else
        {
            totalFixed += middle;
        }
    }
}

Console.WriteLine("Total Correct: " + totalCorrect);
Console.WriteLine("Total Fixed: " + totalFixed);

return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

internal record Constraint(int Before, int After) : IEquatable<Constraint>;

partial class Program
{
    [GeneratedRegex(@"(\d+)\|(\d+)")]
    private static partial Regex ConstraintPattern();
}