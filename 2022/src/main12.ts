import fs from "fs";


function height(c: string) {
    switch (c) {
        case "S":
            c = 'a';
            break;
        case "E":
            c = 'z';
            break;
    }

    return c.charCodeAt(0) - 'a'.charCodeAt(0);
}

function neighbors(lines: string[], unvisited: string[], currNode: string) {
    const [x, y] = currNode.split(",").map(x => parseInt(x, 10));

    return [
        [x - 1, y],
        [x + 1, y],
        [x, y - 1],
        [x, y + 1],
    ].filter(([x1, y1]) => x1 >= 0 && y1 >= 0 && x1 < lines[0].length && y1 < lines.length &&
        height(lines[y][x]) - height(lines[y1][x1]) <= 1 &&
        unvisited.indexOf(`${x1},${y1}`) >= 0
    ).map(([x1, y1]) => `${x1},${y1}`)
}

function nextNode(dist: {[k:string]: number}, unvisited: string[]) {
    let min = Infinity;
    let currNode: string | null = null;
    for (let i = 0; i < unvisited.length; i++) {
        const node = unvisited[i];
        // console.log(node, dist[node], min);
        if (dist[node] < min) {
            min = dist[node];
            currNode = node;
        }
    }

    return currNode;
}

function updateDistances(dist: {[k:string]: number}, currNode: string, ns: string[]) {
    for (let i = 0; i < ns.length; i++) {
        const neighbor = ns[i];
        const newDist = dist[currNode] + 1;
        if (newDist < dist[neighbor]) {
            dist[neighbor] = newDist;
        }
    }

    return dist;
}

function visit(lines: string[], dist: {[k:string]: number}, unvisited: string[], currNode: string) {
    unvisited = unvisited.filter(node => node !== currNode);

    const ns = neighbors(lines, unvisited, currNode);

    dist = updateDistances(dist, currNode, ns);

    return {unvisited, dist};
}

function numberWithSpaces(nr: Number) {

    const str = nr.toString();
    if (str.length === 1) {
        return " " + str + " ";
    } else if (str.length === 2) {
        return " " + str;
    } else {
        return str;
    }
}

function main() {
    const file = fs.readFileSync("12-input-alex.txt").toString();

    // console.log(file);
    const lines = file.split("\n");

    let heights: { [k: string]: number } = {};
    let unvisited: string[] = [];
    let dist: { [k: string]: number } = {};

    let start = "";
    let end = "";
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            unvisited.push(`${x},${y}`);
            dist[`${x},${y}`] = Infinity;
            heights[`${x},${y}`] = height(lines[y][x]);

            if (lines[y][x] === "S") {
                start = `${x},${y}`;
            }

            if (lines[y][x] === "E") {
                end = `${x},${y}`;
            }
        }
    }

    dist[end] = 0;
    while (unvisited.length > 0) {
        const currNode = nextNode(dist, unvisited);

        if (currNode === null) {
            break;
        }

        let {unvisited: u, dist: d} = visit(lines, dist, unvisited, currNode);
        unvisited = u;
        dist = d;
    }

    for (let y = 0; y < lines.length; y++) {
        for (let x = 111; x < lines[y].length; x++) {
            const node = `${x},${y}`;
            if (dist[node] === Infinity) {
                process.stdout.write("    ");
            } else {
                process.stdout.write(numberWithSpaces(dist[node]+1) + " ");
            }
        }
        process.stdout.write("\n");
    }

    for (let y = 0; y < lines.length; y++) {
        for (let x = 111; x < lines[y].length; x++) {
            const node = `${x},${y}`;
            if (dist[node] === Infinity) {
                process.stdout.write("    ");
            } else {
                process.stdout.write("  "+lines[y][x]+" ");
            }
        }
        process.stdout.write("\n");
    }

    console.log(dist[start]);

    let min = Infinity;
    let minNode = "";
    for (let i = 0; i < Object.keys(dist).length; i++) {
        const node = Object.keys(dist)[i];
        if (heights[node] === 0 && dist[node] < min) {
            min = dist[node];
            minNode = node;
        }
    }
    console.log(min);
    // console.log(lines);
    // console.log(unvisited);
    // console.log(dist);
//
}

main();