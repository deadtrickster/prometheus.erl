/* Copyright (c) 2017, Benoit Chesneau <bchesneau@gmail.com>.
 *
 * This file is part of instrument released under the MIT license.
 * See the NOTICE for more information.
 */

#pragma once

#include <atomic>

namespace prometheus {

class Value  {
 public:
  Value();
  Value(double);

  void Increment();
  void Increment(double);
  void Decrement();
  void Decrement(double);
  void Set(double);
  double GetValue() const;

 private:
  void Change(double);
  mutable std::atomic<double> value_;
};

}
