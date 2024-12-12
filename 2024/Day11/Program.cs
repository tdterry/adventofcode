using System.Runtime.CompilerServices;

var numbers = System.IO.File.ReadAllText($"{GetThisFilePath()}/input.txt").Split(" ").Select(long.Parse).ToArray();
var solved = new Dictionary<(long n, int iteration), long>();

Console.WriteLine(numbers.Select(n => Solve(n, 25)).Sum());
Console.WriteLine(numbers.Select(n => Solve(n, 75)).Sum());

return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

long[] NextNumbers(long n)
{
    if (n == 0)
    {
        return [1];
    }
    else if (n.ToString().Length % 2 == 0)
    {
        var s = n.ToString();
        return [long.Parse(s[..(s.Length / 2)]), long.Parse(s[(s.Length / 2)..])];
    }
    else
    {
        return [n * 2024];
    }
}

long Solve(long n, int iteration)
{
    if (iteration == 0)
    {
        return 1;
    }

    if (solved.TryGetValue((n, iteration), out var result)) return result;
    result = NextNumbers(n).Select(n => Solve(n, iteration - 1)).Sum();
    solved[(n, iteration)] = result;

    return result;
}
