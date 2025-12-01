using System.Runtime.CompilerServices;

var input = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt").ToArray();

var towels = input[0].Split(", ").ToList();
var patterns = input[2..];

// Part 1
var knownPatterns = new Dictionary<string, long>();
var total = patterns.Sum(pattern => Matches(pattern, towels, false));
Console.WriteLine(total);

// Part 2
knownPatterns.Clear();
total = patterns.Sum(pattern => Matches(pattern, towels, true));

Console.WriteLine(total);

return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

long Matches(string pattern, List<string> towels, bool computeAll)
{
    if (knownPatterns.TryGetValue(pattern, out var result))
    {
        return result;
    }

    var count = 0L;
    foreach (var towel in towels)
    {
        // Console.WriteLine($"Trying {pattern} with {towel}");
        if (towel == pattern)
        {
            if (computeAll)
            {
                count += 1;
            }
            else
            {
                count = 1;
                break;
            }
        }
        else if (pattern.StartsWith(towel))
        {
            var subPattern = pattern[towel.Length..];
            var subCount = Matches(subPattern, towels, computeAll);
            if (computeAll)
            {
                count += subCount;
            }
            else if (subCount > 0)
            {
                count = 1;
                break;
            }
        }
    }

    knownPatterns[pattern] = count;
    return count;
}