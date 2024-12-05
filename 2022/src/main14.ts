import fs from "fs";

function runSim(grid: Set<string>, maxY: number, floor: boolean) {
    let done = false;
    let count = 0;
    while (!done) {
        let x = 500;
        let y = 0;

        if (grid.has(`${x},${y}`)) {
            break;
        }

        let settled = false;
        while (!settled) {
            if (!grid.has(`${x},${y+1}`)) {
                y++;
            } else if (!grid.has(`${x-1},${y+1}`)) {
                x--;
                y++;
            } else if (!grid.has(`${x+1},${y+1}`)) {
                x++;
                y++;
            } else {
                settled = true;
                grid.add(`${x},${y}`);
                count++;
            }

            if (y > maxY) {
                settled = true;
                if (floor) {
                    grid.add(`${x},${y}`);
                    count++;
                } else {
                    done = true;
                }
            }
        }
    }

    return count;
}

function main() {
    const file = fs.readFileSync("14-input.txt").toString();

    const lines = file.split("\n");

    let grid = new Set<string>();
    for (const line of lines) {
        const points = line.split(" -> ");

        let from = points.shift();
        let to = points.shift();

        while (from && to) {
            let [fx,fy] = from.split(",").map(Number);
            let [tx,ty] = to.split(",").map(Number);

            for (let x = fx, y = fy; x !== tx || y !== ty; x += Math.sign(tx - x), y += Math.sign(ty - y)) {
                grid.add(`${x},${y}`);
            }
            grid.add(`${tx},${ty}`);

            from = to;
            to = points.shift();
        }
    }

    let maxY = -Infinity;
    for (const p of grid) {
        const [_,y] = p.split(",").map(Number);
        maxY = Math.max(maxY, y);
    }

    console.log(runSim(new Set(grid), maxY, false));
    console.log(runSim(new Set(grid), maxY, true));
}

main();