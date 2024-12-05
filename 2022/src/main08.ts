import fs from "fs";


function main() {
    const file = fs.readFileSync("08-input.txt").toString();

    // console.log(file);
    const lines = file.split("\n");


    let visibleCount = 0;
    let maxScore = 0;
    for (let j = 0; j < lines.length; j++) {
        for (let i = 0; i < lines[j].length; i++) {
            // console.log("tree at " + i + "," + j + " is " + lines[j][i]);
            let treeHeight = +lines[j][i];
            // console.log(treeHeight);
            // continue;

            let visible = true;
            let done = false;
            const distances = [];
            for (let dx = -1; dx <= 1; dx++) {
                for (let dy = -1; dy <= 1; dy++) {
                    if (dx !== 0 && dy !== 0 || dx === 0 && dy === 0) {
                        continue;
                    }

                    // console.log("checking direction " + dx + "," + dy);

                    visible = true;
                    let y = j;
                    let x = i;
                    let distance = 0;

                    let doneDirection = false;
                    while (x >= 0 && x < lines[j].length && y >= 0 && y < lines.length && !doneDirection) {
                        x += dx;
                        y += dy;

                        // console.log("checking " + x + ", " + y);
                        if (x >= 0 && x < lines[j].length && y >= 0 && y < lines.length && !doneDirection) {
                            distance++;

                            let otherTreeHeight = +lines[y][x];
                            if (otherTreeHeight >= treeHeight) {
                                if (!done) {
                                    visible = false;
                                }
                                doneDirection = true;
                            }
                        }
                    }

                    distances.push(distance);

                    if (visible) {
                        done = true;
                    }
                }
            }

            const score = distances.reduce((a, b) => a * b, 1);
            // console.log("score at " + i + "," + j + " is " + score);
            if (score > maxScore) {
                maxScore = score;
            }
            if (visible) {
                // console.log("Visible at " + i + "," + j);
                visibleCount++;
            }

            // console.log()
        }
    }

    console.log(visibleCount);
    console.log(maxScore);

}

main();