/*
 * (C) Copyright 2017-2020  UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_VERTCONV_VERTCONV_H_
#define UCLDAS_TRANSFORMS_VERTCONV_VERTCONV_H_

#include <ostream>
#include <string>

#include "ucldas/State/State.h"

#include "oops/util/DateTime.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}
namespace ucldas {
  class Geometry;
  class Increment;
}

// -----------------------------------------------------------------------------

namespace ucldas {

/// UCLDAS linear change of variable
class VertConv: public util::Printable {
 public:
  static const std::string classname() {return "ucldas::VertConv";}

  explicit VertConv(const State &, const State &, const Geometry &,
                    const eckit::Configuration &);
  ~VertConv();

/// Perform linear transforms
  void multiply(const Increment &, Increment &) const;
  void multiplyInverse(const Increment &, Increment &) const;
  void multiplyAD(const Increment &, Increment &) const;
  void multiplyInverseAD(const Increment &, Increment &) const;

 private:
  void print(std::ostream &) const override;
  int keyFtnConfig_;
  const State bkg_lr_;
  const Geometry geom_;
};
// -----------------------------------------------------------------------------

}  // namespace ucldas
#endif  // UCLDAS_TRANSFORMS_VERTCONV_VERTCONV_H_
