//
// Created by Travis on 12/1/25.
//

#include <iostream>
#include <fstream>
#include <ostream>


int main() {
    std::ifstream input("day01.txt");
    std::vector<std::string> lines;

    if (!input) {
        std::cerr << "Failed to open input file" << std::endl;
    }

    for (std::string line; std::getline(input, line);) {
        lines.push_back(line);
    }


    int position = 50;
    int count = 0;
    int part2Count = 0;
    for (const auto &line: lines) {
        printf("dial: %d, move: %s\n", position, line.c_str());

        const auto direction = line[0];
        auto distance = atoi(&line[1]);

        part2Count += distance / 100;
        distance %= 100;
        const auto newPosition = position + (direction == 'L' ? -distance : distance);

        if (newPosition > 99 || (position > 0 && newPosition < 1)) { part2Count++; }

        position = newPosition % 100;
        if (position < 0) position += 100;
        if (position == 0) {
            count++;
        }
        printf("new dial: %d, count=%d part2Count=%d\n", position, count, part2Count);
    }

    printf("Part 1: %d\n", count);
    printf("Part 2: %d\n", part2Count);
    return 0;
}
