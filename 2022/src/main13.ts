import fs from "fs";

type Ord = "LT" | "EQ" | "GT";

function compareLists(parsed1: any, parsed2: any): Ord {
    let ord = "EQ" as Ord;

    // console.log("compareLists", parsed1, parsed2);

    for (let i = 0; i < Math.min(parsed1.length, parsed2.length); i++) {
        const p1Arr = Array.isArray(parsed1[i]);
        const p2Arr = Array.isArray(parsed2[i]);
        if (p1Arr && p2Arr) {
            const subcmp = compareLists(parsed1[i], parsed2[i]);
            if (subcmp !== "EQ") {
                return subcmp;
            }
        } else if (p1Arr) {
            const subcmp = compareLists(parsed1[i], [parsed2[i]]);
            if (subcmp !== "EQ") {
                return subcmp;
            }
        } else if (p2Arr) {
            const subcmp = compareLists([parsed1[i]], parsed2[i]);
            if (subcmp !== "EQ") {
                return subcmp;
            }
        } else {
            if (parsed1[i] < parsed2[i]) {
                return "LT";
            } else if (parsed1[i] > parsed2[i]) {
                return "GT";
            }
        }
    }

    if (parsed1.length < parsed2.length) {
        return "LT";
    } else if (parsed1.length > parsed2.length) {
        return "GT";
    }

    return "EQ";
}

function main() {
    const file = fs.readFileSync("13-input.txt").toString();

    // console.log(file);
    const pairs = file.split("\n\n");
    console.log("lines", pairs);

    let ans1 = 0;
    let i = 1;
    for (const p of pairs) {
        const [p1, p2] = p.split("\n");
        const parsed1 = eval(p1);
        const parsed2 = eval(p2);
        console.log(parsed1, parsed2);

        const cmp = compareLists(parsed1, parsed2);
        if (cmp === "LT") {
            ans1 += i;
        }
        console.log(cmp);
        i++;
    }

    console.log("ans1", ans1);

    const allCodes = file.split("\n").filter(l => !!l).map(p => eval(p));
    allCodes.push([[2]]);
    allCodes.push([[6]]);

    allCodes.sort((a, b) => {
        const cmp = compareLists(a, b);
        if (cmp === "LT") {
            return -1;
        } else if (cmp === "GT") {
            return 1;
        } else {
            return 0;
        }
    });

    console.log("allCodes", allCodes);
    let idx2 = 0;
    let idx6 = 0;
    for (let i = 0; i < allCodes.length; i++) {
        const code = allCodes[i];
        if (code.length == 1 && code[0].length == 1 && code[0][0] == 2) {
            idx2 = i+1;
        } else if (code.length == 1 && code[0].length == 1 && code[0][0] == 6) {
            idx6 = i+1;
        }
    }
    console.log(idx2 * idx6);


}

main();