/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef MAINS_CHECKPOINTMODEL_H_
#define MAINS_CHECKPOINTMODEL_H_

#include <string>

#include "ucldas/Traits.h"

#include "ucldas/Geometry/Geometry.h"
#include "ucldas/Model/Model.h"
#include "ucldas/State/State.h"

#include "eckit/config/LocalConfiguration.h"
#include "oops/base/PostProcessor.h"
#include "oops/mpi/mpi.h"
#include "oops/runs/Application.h"
#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"
#include "oops/util/Logger.h"

namespace ucldas {

  class CheckpointModel : public oops::Application {
   public:
    explicit CheckpointModel(const eckit::mpi::Comm & comm = oops::mpi::world())
      : Application(comm) {}
    static const std::string classname() {return "ucldas::CheckpointModel";}

    int execute(const eckit::Configuration & fullConfig) const {
      //  Setup resolution
      const eckit::LocalConfiguration resolConfig(fullConfig, "resolution");
      const Geometry resol(resolConfig, this->getComm());

      //  Setup Model
      const eckit::LocalConfiguration modelConfig(fullConfig, "model");
      const Model model(resol, modelConfig);

      //  Setup state to write in the restart
      const eckit::LocalConfiguration backgroundConfig(fullConfig,
                                                       "background");
      State xb(resol, backgroundConfig);
      oops::Log::test() << "input background: " << std::endl << xb << std::endl;

      //  Setup state to write in the restart
      const eckit::LocalConfiguration analysisConfig(fullConfig, "analysis");
      State xa(resol, analysisConfig);
      oops::Log::test() << "analysis: " << std::endl << xa << std::endl;

      //  Initialize model
      model.initialize(xb);

      //  Set background to analysis
      xb = xa;

      //  Finalize model (dump restart)
      model.finalize(xb);

      oops::Log::test() << "output background: " << std::endl << xb
                        << std::endl;

      return 0;
    }
    // -----------------------------------------------------------------------------
   private:
    std::string appname() const {
      return "ucldas::CheckpointModel<";
    }
    // -----------------------------------------------------------------------------
  };

}  // namespace ucldas
#endif  // MAINS_CHECKPOINTMODEL_H_
