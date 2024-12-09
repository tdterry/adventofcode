using System.Data;
using System.Runtime.CompilerServices;

var input = System.IO.File.ReadAllLines($"{GetThisFilePath()}/input.txt");
var board = new Dictionary<(int x, int y), char>();
for (var y = 0; y < input.Length; y++)
{
    for (var x = 0; x < input[y].Length; x++)
    {
        board[(x, y)] = input[y][x];
    }
}

System.Console.WriteLine(Part1());
System.Console.WriteLine(Part2());
return;

int Part1()
{
    const string word = "XMAS";
    var count = 0;

    for (var j = -1; j <= 1; j++)
    {
        for (var i = -1; i <= 1; i++)
        {
            if (i == 0 && j == 0) continue;
            foreach (var (x, y) in board.Keys)
            {
                var offset = 0;
                while (offset < word.Length &&
                       board.TryGetValue((x + i * offset, y + j * offset), out var c) &&
                       c == word[offset])
                {
                    offset++;
                }

                if (offset == word.Length)
                {
                    count++;
                }
            }
        }
    }

    return count;
}

int Part2()
{
    var count = 0;
    foreach (var ((x, y), _) in board.Where(p => p.Value == 'A'))
    {
        var values = new string[] { "SM", "MS" };
        if (board.TryGetValue((x - 1, y - 1), out var ul) && board.TryGetValue((x + 1, y + 1), out var br) &&
            values.Contains($"{ul}{br}") &&
            board.TryGetValue((x + 1, y - 1), out var ur) && board.TryGetValue((x - 1, y + 1), out var bl) &&
            values.Contains($"{ur}{bl}")
           )
        {
            count++;
        }
    }

    return count;
}

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;