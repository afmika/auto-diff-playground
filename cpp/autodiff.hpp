#pragma once
#include <cmath>
#include <iostream>
#include <memory>
#include <vector>

#define expr std::shared_ptr<Node>
#define val(x) std::make_shared<Node>(x)
#define val_named(x, name) std::make_shared<Node>(x, name)

class Node {
public:
  float v = 0;
  float g = 0;
  std::string name = "";
  std::vector<expr> deps;

  Node(float v) : v(v) {}
  Node(float v, std::string name) : v(v), name(name) {}

  inline void reset() {
    this->g = 0;
    for (auto &d : deps)
      d->reset();
  }
  inline void backward() { backward(1); }
  virtual void backward(float seed) {
    // everytime this node is referenced through
    // node->backward(deriv), cumulate the derivatives
    // eg. f(x) = x * x => x is referenced 2 times! gx = g_left + g_right
    g += seed;
  }
};

#define BIN_OP(CLS, NAME, OP, DERIVATIVE_A, DERIVATIVE_B)                      \
  class CLS : public Node {                                                    \
  public:                                                                      \
    CLS(expr a, expr b) : Node(OP(a->v, b->v), NAME), a(a), b(b) {             \
      deps = {a, b};                                                           \
    }                                                                          \
    void backward(float seed) override {                                       \
      float av = a->v;                                                         \
      float bv = b->v;                                                         \
      g = seed;                                                                \
      a->backward(DERIVATIVE_A);                                               \
      b->backward(DERIVATIVE_B);                                               \
    }                                                                          \
                                                                               \
  private:                                                                     \
    expr a, b;                                                                 \
  }

#define UNARY_OP(CLS, NAME, OP, DERIVATIVE_A)                                  \
  class CLS : public Node {                                                    \
  public:                                                                      \
    CLS(expr a) : Node(OP(a->v), NAME), a(a) { deps = {a}; }                   \
    void backward(float seed) override {                                       \
      float av = a->v;                                                         \
      g = seed;                                                                \
      a->backward(DERIVATIVE_A);                                               \
    }                                                                          \
                                                                               \
  private:                                                                     \
    expr a;                                                                    \
  }

// MULT
BIN_OP(
    Mult, "*", [&](float x, float y) { return x * y; }, g *bv, g *av);
inline expr operator*(expr left, expr right) {
  return std::make_shared<Mult>(left, right);
}

// ADD
BIN_OP(
    Add, "+", [&](float x, float y) { return x + y; }, 1 * g, 1 * g);
inline expr operator+(expr left, expr right) {
  return std::make_shared<Add>(left, right);
}

// SUB
BIN_OP(
    Sub, "-", [&](float x, float y) { return x - y; }, -1 * g, -1 * g);
inline expr operator-(expr left, expr right) {
  return std::make_shared<Sub>(left, right);
}

// NEG
UNARY_OP(
    Neg, "neg", [&](float x) { return -x; }, -1 * g);
inline expr operator-(expr left) { return std::make_shared<Neg>(left); }

// POW
BIN_OP(
    Pow, "^", [&](float x, float y) { return pow(x, y); }, bv *av *(bv - 1) * g,
    1 * g);
inline expr pow(expr left, expr right) {
  return std::make_shared<Pow>(left, right);
}

// INV + DIV
UNARY_OP(
    Inv, "inv", [&](float x) { return 1 / x; }, (-1 / (av * av)) * g);
inline expr inv(expr left) { return std::make_shared<Inv>(left); }
inline expr operator/(expr left, expr right) { return left * inv(right); }

// SIGMOID
inline float _sigmoid(float x) { return 1 / (1 + exp(-x)); }
UNARY_OP(Sigmoid, "sigmoid", _sigmoid, _sigmoid(av) * (1 - _sigmoid(av)) * g);
inline expr sigmoid(expr left) { return std::make_shared<Sigmoid>(left); }

// ReLU
UNARY_OP(
    ReLU, "ReLU", [&](float x) { return x > 0 ? x : 0; }, (av > 0 ? 1 : 0) * g);
inline expr relu(expr left) { return std::make_shared<ReLU>(left); }

// Display
std::ostream &operator<<(std::ostream &os, const Node &value) {
  os << "[v=" << value.v << ", g=" << value.g << "]";
  return os;
}

std::ostream &operator<<(std::ostream &os, const expr &value) {
  os << value->name << (value->name.empty() ? "" : " ") << "[v=" << value->v
     << ", g=" << value->g << "]";
  return os;
}

void tree(expr x, int depth = 0) {
  std::string spaces = "";
  spaces.resize(2 * depth, ' ');

  if (x->deps.empty()) {
    std::cout << x << '\n';
    return;
  }

  std::cout << x << '\n';

  for (auto it = x->deps.begin(); it != x->deps.end(); it++) {
    auto index = it - x->deps.begin();
    std::cout << spaces << " |- c" << index << ' ';
    tree(*it, depth + 1);
  }
}
