#include <stdio.h>
#include <oxidize/oxidize.h>

typedef struct Person
{
    char *name;
    int age;
} Person;

double fahrenheit(double celsius)
{
    return 32. + celsius * 9. / 5.;
}

int main()
{
    const Person zahash = {"zahash", 24};
    printf("%s\n", zahash.name);
    printf("%d\n", zahash.age);

    printf("(0c = 32f) %f\n", fahrenheit(0.));
    printf("(100c = 212f) %f\n", fahrenheit(100.));
    printf("(69c = 156.2f) %f\n", fahrenheit(69.));
    printf("(69.420c = 156.956f) %f\n", fahrenheit(69.420));

    printf("%d\n", plus_one(1));

    return 0;
}
