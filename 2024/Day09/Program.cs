using System.Diagnostics;
using System.Runtime.CompilerServices;

var input = System.IO.File.ReadAllText($"{GetThisFilePath()}/input.txt");
var size = input.Select(c => c - '0').Sum();


// Part 1
{
    var disk = new long[size];
    var id = 0;
    var active = true;
    var index = 0;
    foreach (var c in input)
    {
        for (var i = 0; i < c - '0'; i++)
        {
            disk[index++] = active ? id : -1;
        }

        if (active)
        {
            id++;
        }

        active = !active;
    }

    // last block is alway used, find the first empty block
    var lastIndex = disk.Length - 1;
    index = 0;
    while (disk[index] != -1)
    {
        index++;
    }

    while (index < lastIndex)
    {
        disk[index] = disk[lastIndex];
        disk[lastIndex] = -1;
        while (lastIndex > index && disk[lastIndex] == -1)
        {
            lastIndex--;
        }

        while (index < lastIndex && disk[index] != -1)
        {
            index++;
        }
    }

    var checksum = disk.Where(c => c > -1).Select((c, idx) => c * idx).Sum();
    Console.WriteLine(checksum);
}

// Part 2
{
    var blocks = input.Aggregate((true, 0, new List<Block>()), (acc, next) =>
    {
        var (active, id, blocks) = acc;
        blocks.Add(active ? new Block.UsedBlock(id, next - '0') : new Block.FreeBlock(next - '0'));
        return (!active, active ? id + 1 : id, blocks);
    }).Item3;

    var moveIndex = blocks.Count - 1;
    while (moveIndex > 0)
    {
        // Console.Write("blocks: ");
        // WriteBlocks(blocks);
        // Console.WriteLine($"Attempting to move block at index {moveIndex} with id {blocks[moveIndex].id} and size {blocks[moveIndex].size}");
        if (blocks[moveIndex] is Block.FreeBlock)
        {
            moveIndex--;
            continue;
        }

        var idx = blocks.FindIndex(b => b switch
        {
            Block.FreeBlock fb => b.Size >= blocks[moveIndex].Size,
            _ => false,
        });
        if (idx == -1 || idx >= moveIndex)
        {
            moveIndex--;
            continue;
        }

        var b = blocks[moveIndex];
        blocks[moveIndex] = new Block.FreeBlock(b.Size);
        moveIndex--;
        blocks.Insert(idx, b);
        if (blocks[idx + 1].Size > b.Size)
        {
            blocks[idx + 1] = new Block.FreeBlock(blocks[idx + 1].Size - b.Size);
        }
        else
        {
            blocks.RemoveAt(idx + 1);
        }

        // consolidate empty blocks
        for (var i = moveIndex; i < blocks.Count - 1; i++)
        {
            while (i < blocks.Count - 1 && blocks[i] is Block.FreeBlock && blocks[i + 1] is Block.FreeBlock)
            {
                blocks[i] = new Block.FreeBlock(blocks[i].Size + blocks[i + 1].Size);
                blocks.RemoveAt(i + 1);
            }
        }
    }

// WriteBlocks(blocks);

    var index = 0;
    var disk = new long[size];
    foreach (var b in blocks)
    {
        for (var j = 0; j < b.Size; j++)
        {
            disk[index++] = b switch
            {
                Block.UsedBlock ub => ub.Id,
                _ => 0,
            };
        }
    }

    var checksum = disk.Select((c, idx) => c > -1 ? c * idx : 0).Sum();
    Console.WriteLine(checksum);
}


return;

string GetThisFilePath([CallerFilePath] string path = "") => Path.GetDirectoryName(path)!;

void WriteBlocks(List<(bool free, long id, long size)> blocks)
{
    foreach (var b in blocks)
    {
        Console.Write(b.id > -1 ? new string((char)(b.id + '0'), (int)b.size) : new string('.', (int)b.size));
        Console.Write(" ");
    }

    Console.WriteLine();
}

internal abstract record Block(long Size)
{
    internal record FreeBlock(long Size) : Block(Size);

    internal record UsedBlock(long Id, long Size) : Block(Size);
}