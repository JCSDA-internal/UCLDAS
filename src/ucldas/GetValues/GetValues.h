/*
 * (C) Copyright 2019-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_GETVALUES_GETVALUES_H_
#define UCLDAS_GETVALUES_GETVALUES_H_

#include <memory>
#include <ostream>
#include <string>

#include "ucldas/Fortran.h"

#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

#include "ufo/Locations.h"

// Forward declarations
namespace oops {
  class Variables;
}

namespace ufo {
  class GeoVaLs;
}
namespace ucldas {
  class Geometry;
  class State;
  class Model2GeoVaLs;
}

//-----------------------------------------------------------------------------

namespace ucldas {

  /// UCLDAS GetValues
  /*!
   * GetValues: interpolate State to observation locations
   */
class GetValues : public util::Printable,
                    private util::ObjectCounter<GetValues> {
 public:
  static const std::string classname() {return "ucldas::GetValues";}

/// saves all locations locs to use during filling GeoVaLs
  GetValues(const Geometry &, const ufo::Locations & locs,
            const eckit::Configuration & config);
  virtual ~GetValues();

  /// fills in geovals for all observations in the timeframe (t1, t2],
  /// geovals are interpolated trilinearly from state at the nearest gridpoints
  void fillGeoVaLs(const State &,
                   const util::DateTime & t1,
                   const util::DateTime & t2,
                   ufo::GeoVaLs &) const;

  /// Read interpolated GeoVaLs at observation location
  void getValuesFromFile(const ufo::Locations &,
                         const oops::Variables &,
                         ufo::GeoVaLs &) const;

 private:
  void print(std::ostream &) const;
  F90getval keyGetValues_;
  ufo::Locations locs_;
  std::shared_ptr<const Geometry> geom_;
  std::unique_ptr<Model2GeoVaLs> model2geovals_;
};
// -----------------------------------------------------------------------------

}  // namespace ucldas

#endif  // UCLDAS_GETVALUES_GETVALUES_H_
