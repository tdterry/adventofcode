//
// Created by Travis on 12/1/25.
//

#include <iostream>
#include <fstream>
#include <ostream>
#include <regex>

std::tuple<long, std::string> joltage2(std::string const &digits, int len) {
    auto max_element = std::max_element(digits.begin(), digits.end()-len);
    auto max_index = max_element - digits.begin();

    return {*max_element - '0', std::string(digits.begin() + max_index + 1, digits.end())};
}

long joltage(std::string const &s, const int digits) {
    long j = 0;
    auto r = std::tuple<int, std::string>{0, s};
    for (int i = digits - 1; i >= 0; --i) {
        r = joltage2(std::get<1>(r), i);
        j = j * 10 + std::get<0>(r);
    }
    return j;
}

int main() {
    std::ifstream input("day03.txt");
    std::vector<std::string> lines;

    if (!input) {
        std::cerr << "Failed to open input file" << std::endl;
    }

    for (std::string line; std::getline(input, line);) {
        lines.push_back(line);
    }

    long part1_total = 0;
    long part2_total = 0;
    for (const auto &line: lines) {
        part1_total += joltage(line, 2);
        part2_total += joltage(line, 12);
    }

    std::cout << "Part 1: " << part1_total << std::endl;
    std::cout << "Part 2: " << part2_total << std::endl;

    return 0;
}
