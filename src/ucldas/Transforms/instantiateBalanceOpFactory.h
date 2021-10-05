/*
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef UCLDAS_TRANSFORMS_INSTANTIATEBALANCEOPFACTORY_H_
#define UCLDAS_TRANSFORMS_INSTANTIATEBALANCEOPFACTORY_H_

#include "oops/base/VariableChangeBase.h"
#include "oops/interface/LinearVariableChange.h"

#include "ucldas/Traits.h"
#include "ucldas/Transforms/Ana2Model/Ana2Model.h"
#include "ucldas/Transforms/Balance/Balance.h"
#include "ucldas/Transforms/BkgErr/BkgErr.h"
#include "ucldas/Transforms/BkgErrFilt/BkgErrFilt.h"
#include "ucldas/Transforms/BkgErrGodas/BkgErrGodas.h"
#include "ucldas/Transforms/HorizFilt/HorizFilt.h"
#include "ucldas/Transforms/VertConv/VertConv.h"

namespace ucldas {

void instantiateBalanceOpFactory() {
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::VertConv> >
              makerBalanceOpVertConvUCLDAS_("VertConvUCLDAS");
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::BkgErr> >
              makerBalanceOpBkgErrUCLDAS_("BkgErrUCLDAS");
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::BkgErrGodas> >
              makerBalanceOpBkgErrGODAS_("BkgErrGODAS");
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::BkgErrFilt> >
              makerBalanceOpBkgErrFILT_("BkgErrFILT");
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::Balance> >
              makerBalanceOpBalanceUCLDAS_("BalanceUCLDAS");
  static oops::LinearVariableChangeMaker<ucldas::Traits,
              oops::LinearVariableChange<ucldas::Traits, ucldas::HorizFilt> >
              makerBalanceOpHorizFILT_("HorizFiltUCLDAS");
  static oops::VariableChangeMaker<ucldas::Traits, ucldas::Ana2Model >
              makerAna2Model_("Ana2Model");
}
}  // namespace ucldas

#endif  // UCLDAS_TRANSFORMS_INSTANTIATEBALANCEOPFACTORY_H_
