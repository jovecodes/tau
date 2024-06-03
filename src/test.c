#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void println(const char * str) {
    printf(str);

}

void takes_reference(int* x);

void takes_pointer(int* x) {
    if (x == NULL) {
        println("Thats null!");
    }
    else if (*x == 2) {
        println("Wow it's 2!");
    }
    else {
        println("{*x}");
    }
}

typedef struct Point {
    float x;
    float y;
} Point;

int square(int x) {
    Point p = (Point) {
        .x = 10,
        .y = 20,
    }
    return x * x;
}

int main() {
    int Point = 10;

    int x = 10;
    int y;
    if (x == 2) {
        y = 3;
    } else if (x == 3) {
        y = 9;
    } else {
        y = 5;
    }
    return 0;
}
