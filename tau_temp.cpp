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
const bool maybe = false;
const String msg = "Nope";
if (true) {
println(msg);
}
;
print("x: ");
println(str(t.min.x));
int xs [3] = {1,2,3,};
}

