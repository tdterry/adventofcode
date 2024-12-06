// See https://aka.ms/new-console-template for more information

using System.Text.RegularExpressions;
using System.Runtime.CompilerServices;

var lines = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt");
var reports = lines.Select(l => l.Split(" ").Select(int.Parse).ToArray()).ToArray();
Console.WriteLine(reports.Where(IsSafe).Count());
Console.WriteLine(reports.Where(IsSafeWithRemoval).Count());

return;

bool IsSafeWithRemoval(int[] levels)
{
    if (IsSafe(levels))
    {
        return true;
    }

    for (var i = 0; i < levels.Length; i++)
    {
        var copy = levels.ToList();
        copy.RemoveAt(i);
        if (IsSafe(copy.ToArray()))
        {
            return true;
        }
    }

    return false;
}

bool IsSafe(int[] levels)
{
    if (levels[1] == levels[0])
    {
        return false;
    }

    var ascending = levels[1] > levels[0];
    return levels[1..].Aggregate((curr: levels[0], ok: true), (acc, next) =>
    {
        var diff = next - acc.curr;
        var ok = acc.ok && Math.Abs(diff) <= 3 && (ascending && diff > 0 || !ascending && diff < 0);
        return (next, ok); 
    }).ok;

    return true;
}

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;