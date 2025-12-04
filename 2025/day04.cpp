//
// Created by Travis on 12/1/25.
//

#include <iostream>
#include <fstream>
#include <ostream>
#include <regex>
#include <set>

int main() {
    std::ifstream input("day04.txt");
    std::vector<std::string> lines;

    if (!input) {
        std::cerr << "Failed to open input file" << std::endl;
    }

    for (std::string line; std::getline(input, line);) {
        lines.push_back(line);
    }

    std::set<std::tuple<int, int> > rolls;
    for (auto line = lines.begin(); line != lines.end(); ++line) {
        for (auto ch = line->begin(); ch != line->end(); ++ch) {
            if (*ch == '@') {
                rolls.insert({line - lines.begin(), ch - line->begin()});
            }
        }
    }

    unsigned long part1_total = 0;
    unsigned long part2_total = 0;
    bool first_pass = true;
    const std::set<std::tuple<int, int> > dirs{
        {-1, -1}, {0, -1}, {1, -1},
        {-1, 0}, {1, 0},
        {-1, 1}, {0, 1}, {1, 1},
    };
    while (true) {
        std::set<std::tuple<int, int> > can_remove;
        for (const auto &pos: rolls) {
            int neighbors = 0;
            for (const auto &dir: dirs) {
                const auto x = std::get<0>(pos) + std::get<0>(dir);
                const auto y = std::get<1>(pos) + std::get<1>(dir);
                if (rolls.find({x, y}) != rolls.end()) {
                    neighbors++;
                }
            }
            if (neighbors < 4) {
                can_remove.insert(pos);
            }
        }

        if (first_pass) {
            part1_total = can_remove.size();
            first_pass = false;
        }
        part2_total += can_remove.size();

        if (can_remove.empty()) {
            break;
        }

        for (const auto &pos: can_remove) {
            rolls.erase(pos);
        }
    }

    std::cout << "Part 1: " << part1_total << std::endl;
    std::cout << "Part 2: " << part2_total << std::endl;

    return 0;
}
