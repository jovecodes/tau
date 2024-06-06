#include <cstdio>
#include <Jovial/Std/String.h>
#include <Jovial/Std/Print.h>
using namespace jovial;

void print(const String &s) { printf("%s", s.utf8().get_data()); }
void println(const String &s) { printf("%s\n", s.utf8().get_data()); }

void test() {
if ("cpp" == "cpp") {
}
;
}

float sqrt(float x) {
return x;
}

;

typedef struct Point {
float x;
float y;
} Point;

typedef struct Rect {
Point min;
Point max;
} Rect;

void say_hello() {
println("Hello");
}

int main() {
Rect t = (Rect) {.min = (Point) {.x = 1, .y = 2, }, .max = (Point) {.x = 3, .y = 4, }, };
say_hello();
String msg;
if (true) {
msg = "Yup";
}
else {
msg = "Nope";
}
;
if (true) {
println(msg);
}
;
}

