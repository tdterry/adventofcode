import fs from "fs";

interface File {
    name: string;
    size: number;
}

interface Dir {
    name: string;
    subdirs: Dir[];
    files: File[];
}

const root: Dir = {
    name: "/",
    subdirs: [],
    files: []
};
function parse(ctx: Dir[], dir: Dir, line: string) {
    if (line == "$ cd /") {
        dir = root;
        ctx.push(dir);
    } else if (line == "$ cd ..") {
        dir = ctx.pop()!;
    } else if (line.startsWith("$ cd ")) {
        const name = line.slice(5);

        const subdir: Dir = {
            name,
            subdirs: [],
            files: []
        };

        dir!.subdirs.push(subdir);
        ctx.push(dir!);
        dir = subdir;
    } if (!line.startsWith("$") && !line.startsWith("dir ")) {
        const parts = line.split(" ");
        const size = parseInt(parts[0]);
        const name = parts[1];
        const file: File = {
            name,
            size
        };
        dir.files.push(file);
    }
    return dir;

}

function walkDir(dir: Dir, indent: string) {
    console.log(indent + dir.name);
    for (const subdir of dir.subdirs) {
        walkDir(subdir, indent + "  ");
    }
    for (const file of dir.files) {
        console.log(indent + "  " + file.name + " (file, size: " + file.size + ")");
    }
}

function dirSize(dir: Dir) {
    let size = 0;
    for (const subdir of dir.subdirs) {
        size += dirSize(subdir).size;
    }
    for (const file of dir.files) {
        size += file.size;
    }
    return {name: dir.name, size};
}

function dirSizes(dir: Dir): {name:string, size:number}[] {
    const sizes = [dirSize(dir)];
    for (const subdir of dir.subdirs) {
        sizes.push(...dirSizes(subdir));
    }
    return sizes;
}

function main() {
    const file = fs.readFileSync("07-input.txt").toString();

    const lines = file.split("\n");
    // console.log(lines);

    let ctx: Dir[] = [];
    let dir: Dir;
    for (const line of lines) {
        // console.log(line);
        dir = parse(ctx, dir!, line);
    }

    // console.log(JSON.stringify(root, null, 2));

    // walkDir(ctx[0], "");

    // console.log("Total size: " + dirSize(root));
    //
    // console.log("Sizes: " + JSON.stringify(dirSizes(root), null, 4));

    console.log(dirSizes(root).filter(s => s.size <= 100000).reduce((a, b) => a + b.size, 0));

    const capacity = 70000000;
    const requiredFreeSpace = 30000000;
    const freeSpace = capacity - dirSize(root).size;
    // console.log("Free space: " + freeSpace);

    const sizes = dirSizes(root).sort((a, b) => a.size - b.size);
    // console.log(sizes);

    console.log(sizes.filter(s => s.size >= requiredFreeSpace - freeSpace)[0].size);
}

main();