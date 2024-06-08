#include <cstdio>
#include <Jovial/Std/String.h>
#include <Jovial/Std/Print.h>
using namespace jovial;

void print(String s) {
{
printf("%s", s.utf8().get_data());;
}
;
}

void println(String s) {
{
printf("%s\n", s.utf8().get_data());;
}
;
}

String str(int self) {
{
return to_string(self);;
}
;
}

float sqrt(float x) {
}

;

typedef struct Point {
const float x;
const float y;
} Point;

typedef struct Rect {
const Point min;
const Point max;
} Rect;

void say_hello() {
println("Hello");
}

int main() {
const Rect t = {.min = {.x = 1, .y = 2, }, .max = {.x = 3, .y = 4, }, };
say_hello();
const bool maybe = false;
const String msg = "Nope";
if (true) {
println(msg);
}
;
print("x: ");
println(str(t.min.x));
Vec<int> xs = {1,2,3,4,};
}

