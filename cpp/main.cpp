#include "autodiff.hpp"
#include <algorithm>
#include <cassert>
#include <ctime>
#include <iostream>
#include <random>

inline expr f(expr x, expr y) { return x * (x + y) + y * y; }

void basicExample() {
  auto x = val_named(2, "x");
  auto y = val_named(3, "y");
  auto z = f(x, y);

  z->backward();

  tree(z);

  z->name = "z";
  std::cout << z << std::endl;
  std::cout << x << std::endl;
  std::cout << y << std::endl;
}

void linearReg() {
  std::default_random_engine engine(static_cast<long unsigned int>(time(0)));
  std::uniform_real_distribution<float> distr(0., 1.);

  // dataset
  std::vector<std::pair<expr, expr>> points = {
      // Apple
      {val(1), val(2)},
      {val(2), val(2.5)},
      {val(2), val(3.5)},
      // Orange
      {val(2), val(1.5)},
      {val(1), val(1)},
      {val(4), val(3.5)},
  };

  // y = ax + b, find a, b such that E(a, b) is minimal

  int steps = 100;
  float rate = 0.1;
  expr a = val_named(distr(engine), "A");
  expr b = val_named(distr(engine), "B");

  for (int i = 1; i <= steps; i++) {
    expr err = val(0);
    for (const auto &[xi, yi] : points) {
      err = err + pow(yi - (a * xi + b), val(2));
    }
    err = err / val((float)points.size());
    // computes dE/da, dE/db
    // err->reset();
    err->backward();

    std::cout << "Step " << i << ":" << err->v << '\n';

    // update
    a = val_named(a->v - a->g * rate, a->name);
    b = val_named(b->v - b->g * rate, b->name);
  }
  std::cout << "y = " << a->v << "x + " << b->v;
}

int main() {
#ifdef BASIC
  basicExample();
#else
  linearReg();
#endif
  return 0;
}