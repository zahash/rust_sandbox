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

typedef enum MyEnum_Tag {
  A,
  B,
  C,
  D,
} MyEnum_Tag;

typedef struct B_Body {
  float _0;
  uint64_t _1;
} B_Body;

typedef struct C_Body {
  uint32_t x;
  uint8_t y;
} C_Body;

typedef struct MyEnum {
  MyEnum_Tag tag;
  union {
    struct {
      uint32_t a;
    };
    B_Body b;
    C_Body c;
  };
} MyEnum;

void use_person(const struct Person *_p);

void use_coords(const struct Coords *_c);

void use_my_enum(const struct MyEnum *_e);

int plus_one(int a);

void greet(const char *s);
