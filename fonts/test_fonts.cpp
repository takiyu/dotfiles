#include <iostream>


template <typename T>
void Print(T&& t) {
    std::cout << t << std::endl;
}

int main(int argc, char const* argv[]) {
    Print("Hello World");
    Print("0123456789");
    Print("あいうえお");
    Print("アイウエオ");
    Print("日本語");
    Print("☃ 1⃣ ↔ ☺");
    Print("🌀⏪🌝😊😖😣😄🙆");
    Print("      ");
    return 0;
}
