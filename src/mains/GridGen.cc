/*
 * (C) Copyright 2017-2021 UCAR.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include "mains/GridGen.h"
#include "oops/runs/Run.h"
#include "ucldas/Traits.h"


int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  ucldas::GridGen makegrid;
  return run.execute(makegrid);
}
