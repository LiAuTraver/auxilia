#include <atomic>
#include <iostream>
#include <ostream>
#include <thread>
volatile int vc(0);
std::atomic_int ac(0);

auto modification() {
  ac++;
  vc = vc + 1;
}
int main() {
  {
    std::jthread j1(modification), j2(modification);
  }
  std::cout << "ac: " << ac << std::endl;
  std::cout << "vc: " << vc << std::endl;
}
