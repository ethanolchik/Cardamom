#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cstdlib>

void println(std::string s) {
    std::cout << s << std::endl;
}
std::string input() {
    std::string line;
    std::getline(std::cin, line);
    return line;
}
int len(std::string s) {
    return s.size();
}
char charAt(std::string s, int i) {
    return s.at(i);
}
std::string toString(int s) {
    return std::to_string(s);
}
int charCodeAt(std::string s, int i) {
    return s.at(i);
}

std::string fromASCII(int ascii) {
    std::string s = "";
    s += char(ascii);
    return s;
}

void brainfuck(std::string code)
{
    std::vector<int> tape = {0};
    int ptr = 0;
    int code_len = len(code);
    int code_ptr = 0;
    std::vector<int> loop_stack = {0};
    while ((code_ptr < code_len))     {
        char command = charAt(code, code_ptr);
        if ((command == '>'))         {
            ptr += 1;
        }
        else 
            if ((command == '<'))             {
                ptr -= 1;
            }
            else 
                if ((command == '+'))                 {
                    tape[ptr] += 1;
                }
                else 
                    if ((command == '-'))                     {
                        tape[ptr] -= 1;
                    }
                    else 
                        if ((command == '.'))                         {
                            println(fromASCII(tape[ptr]));
                        }
                        else 
                            if ((command == ','))                             {
                                std::string input_char = input();
                                if ((len(input_char) > 0))                                 {
                                    tape[ptr] = charCodeAt(input_char, 0);
                                }
                            }
                            else 
                                if ((command == '['))                                 {
                                    if ((tape[ptr] == 0))                                     {
                                        int open_brackets = 1;
                                        while ((open_brackets > 0))                                         {
                                            code_ptr += 1;
                                            if ((charAt(code, code_ptr) == '['))                                             {
                                                open_brackets += 1;
                                            }
                                            else 
                                                if ((charAt(code, code_ptr) == ']'))                                                 {
                                                    open_brackets -= 1;
                                                }
                                        }
                                    }
                                    else                                     {
                                        loop_stack.push_back(code_ptr);
                                    }
                                }
                                else 
                                    if ((command == ']'))                                     {
                                        if ((tape[ptr] != 0))                                         {
                                            code_ptr = loop_stack[(loop_stack.size() - 1)];
                                        }
                                        else                                         {
                                            loop_stack.pop_back();
                                        }
                                    }
        code_ptr += 1;
    }
}
int main()
{
    println("Enter bf code: ");
    std::string x = input();
    brainfuck(x);

    return 0;
}
