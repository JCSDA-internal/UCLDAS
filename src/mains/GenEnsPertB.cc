/*
 * (C) Copyright 2017-2020 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include "oops/generic/instantiateModelFactory.h"
#include "oops/runs/GenEnsPertB.h"
#include "oops/runs/Run.h"
#include "ucldas/Traits.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  oops::instantiateModelFactory<ucldas::Traits>();
  oops::GenEnsPertB<ucldas::Traits> ensgen;
  return run.execute(ensgen);
}
