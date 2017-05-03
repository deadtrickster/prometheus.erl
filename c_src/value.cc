/* Copyright (c) 2017, Benoit Chesneau <bchesneau@gmail.com>.
 *
 * This file is part of instrument released under the MIT license.
 * See the NOTICE for more information.
 */

#include "value.h"

namespace prometheus {

Value::Value() : value_{0} {}

Value::Value(double value) : value_{value} {}

void Value::Increment() { Increment(1.0); }
void Value::Increment(double value) {
  if (value < 0.0) {
    return;
  }
  Change(value);
}

void Value::Decrement() { Decrement(1.0); }

void Value::Decrement(double value) {
  if (value < 0.0) {
    return;
  }
  Change(-1.0 * value);
}

void Value::Set(double value) { value_.store(value); }

void Value::Change(double value) {
  auto current = value_.load();
  while (!value_.compare_exchange_weak(current, current + value))
    ;
}

double Value::GetValue() const { return value_; }

}
