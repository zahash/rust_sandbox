#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Person {
  const char *name;
  int age;
} Person;

typedef struct Coords {
  double x;
  double y;
} Coords;

void use_person(const struct Person *p);

void use_coords(const struct Coords *c);

int plus_one(int a);

void greet(const char *s);
