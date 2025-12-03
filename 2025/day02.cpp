//
// Created by Travis on 12/1/25.
//

#include <iostream>
#include <fstream>
#include <ostream>
#include <regex>

bool is_silly_by_repeats(std::string const &s, const long repeats) {
    if (s.size() % repeats != 0) { return false; }
    const auto len = s.size() / repeats;
    for (auto i = len; i <= s.size() - len; i += len) {
        if (s.substr(i, len) != s.substr(0, len)) {
            return false;
        }
    }
    return true;
}

bool is_silly(std::string const &s) {
    for (auto repeats = 2; repeats <= s.size(); ++repeats) {
        if (is_silly_by_repeats(s, repeats)) {
            return true;
        }
    }

    return false;
}

int main() {
    std::ifstream input("day02.txt");
    std::vector<std::string> lines;
    std::regex pattern("(\\d+)-(\\d+)");

    if (!input) {
        std::cerr << "Failed to open input file" << std::endl;
    }

    for (std::string line; std::getline(input, line, ',');) {
        lines.push_back(line);
    }

    long twice_total = 0;
    long all_total = 0;
    for (const auto &line: lines) {
        std::smatch matches;
        std::regex_match(line, matches, pattern);
        const auto min = std::stol(matches[1]);
        const auto max = std::stol(matches[2]);

        for (auto i = min; i <= max; ++i) {
            if (is_silly_by_repeats(std::to_string(i), 2)) {
                twice_total += i;
            }

            if (is_silly(std::to_string(i))) {
                all_total += i;
            }
        }
    }

    std::cout << "Part 1: " << twice_total << std::endl;
    std::cout << "Part 2: " << all_total << std::endl;

    return 0;
}
