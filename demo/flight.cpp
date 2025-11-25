#include <cassert>
#include <iostream>
#include <new>
struct B;
struct A {
  int i = 0;
  B *toB();
};
struct B : public A {};
B *A::toB() { return new (this) B; }

static_assert(sizeof(A) == sizeof(B));
// ok if both trivial and standard layout
int main() {
  A a;
  std::cout << a.i;
  auto b = a.toB();
  std::cout << b->i;
  assert(&a == b);
}
