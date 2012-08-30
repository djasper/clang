//===--- AnalyzerOptions.h - Analysis Engine Options ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header defines various options for the static analyzer that are set
// by the frontend and are consulted throughout the analyzer.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_ANALYZEROPTIONS_H
#define LLVM_CLANG_ANALYZEROPTIONS_H

#include <string>
#include <vector>
#include "llvm/ADT/StringMap.h"

namespace clang {
class ASTConsumer;
class DiagnosticsEngine;
class Preprocessor;
class LangOptions;

/// Analysis - Set of available source code analyses.
enum Analyses {
#define ANALYSIS(NAME, CMDFLAG, DESC, SCOPE) NAME,
#include "clang/StaticAnalyzer/Analyses.def"
NumAnalyses
};

/// AnalysisStores - Set of available analysis store models.
enum AnalysisStores {
#define ANALYSIS_STORE(NAME, CMDFLAG, DESC, CREATFN) NAME##Model,
#include "clang/StaticAnalyzer/Analyses.def"
NumStores
};

/// AnalysisConstraints - Set of available constraint models.
enum AnalysisConstraints {
#define ANALYSIS_CONSTRAINTS(NAME, CMDFLAG, DESC, CREATFN) NAME##Model,
#include "clang/StaticAnalyzer/Analyses.def"
NumConstraints
};

/// AnalysisDiagClients - Set of available diagnostic clients for rendering
///  analysis results.
enum AnalysisDiagClients {
#define ANALYSIS_DIAGNOSTICS(NAME, CMDFLAG, DESC, CREATFN, AUTOCREAT) PD_##NAME,
#include "clang/StaticAnalyzer/Analyses.def"
NUM_ANALYSIS_DIAG_CLIENTS
};

/// AnalysisPurgeModes - Set of available strategies for dead symbol removal.
enum AnalysisPurgeMode {
#define ANALYSIS_PURGE(NAME, CMDFLAG, DESC) NAME,
#include "clang/StaticAnalyzer/Analyses.def"
NumPurgeModes
};

/// AnalysisIPAMode - Set of inter-procedural modes.
enum AnalysisIPAMode {
#define ANALYSIS_IPA(NAME, CMDFLAG, DESC) NAME,
#include "clang/StaticAnalyzer/Analyses.def"
NumIPAModes
};

/// AnalysisInlineFunctionSelection - Set of inlining function selection heuristics.
enum AnalysisInliningMode {
#define ANALYSIS_INLINING_MODE(NAME, CMDFLAG, DESC) NAME,
#include "clang/StaticAnalyzer/Analyses.def"
NumInliningModes
};

class AnalyzerOptions {
public:
  typedef llvm::StringMap<std::string> ConfigTable;

  /// \brief Pair of checker name and enable/disable.
  std::vector<std::pair<std::string, bool> > CheckersControlList;
  
  /// \brief A key-value table of use-specified configuration values.
  ConfigTable Config;
  AnalysisStores AnalysisStoreOpt;
  AnalysisConstraints AnalysisConstraintsOpt;
  AnalysisDiagClients AnalysisDiagOpt;
  AnalysisPurgeMode AnalysisPurgeOpt;
  
  // \brief The interprocedural analysis mode.
  AnalysisIPAMode IPAMode;
  
  std::string AnalyzeSpecificFunction;
  
  /// \brief The maximum number of exploded nodes the analyzer will generate.
  unsigned MaxNodes;
  
  /// \brief The maximum number of times the analyzer visits a block.
  unsigned MaxLoop;
  
  
  unsigned ShowCheckerHelp : 1;
  unsigned AnalyzeAll : 1;
  unsigned AnalyzerDisplayProgress : 1;
  unsigned AnalyzeNestedBlocks : 1;
  
  /// \brief The flag regulates if we should eagerly assume evaluations of
  /// conditionals, thus, bifurcating the path.
  ///
  /// EagerlyAssume - A flag indicating how the engine should handle
  ///   expressions such as: 'x = (y != 0)'.  When this flag is true then
  ///   the subexpression 'y != 0' will be eagerly assumed to be true or false,
  ///   thus evaluating it to the integers 0 or 1 respectively.  The upside
  ///   is that this can increase analysis precision until we have a better way
  ///   to lazily evaluate such logic.  The downside is that it eagerly
  ///   bifurcates paths.
  unsigned EagerlyAssume : 1;
  
  unsigned TrimGraph : 1;
  unsigned VisualizeEGDot : 1;
  unsigned VisualizeEGUbi : 1;
  unsigned UnoptimizedCFG : 1;
  unsigned CFGAddImplicitDtors : 1;
  unsigned eagerlyTrimExplodedGraph : 1;
  unsigned PrintStats : 1;
  
  /// \brief Do not re-analyze paths leading to exhausted nodes with a different
  /// strategy. We get better code coverage when retry is enabled.
  unsigned NoRetryExhausted : 1;
  
  /// \brief The inlining stack depth limit.
  unsigned InlineMaxStackDepth;
  
  /// \brief The mode of function selection used during inlining.
  unsigned InlineMaxFunctionSize;

  /// \brief The mode of function selection used during inlining.
  AnalysisInliningMode InliningMode;

public:
  AnalyzerOptions() {
    AnalysisStoreOpt = RegionStoreModel;
    AnalysisConstraintsOpt = RangeConstraintsModel;
    AnalysisDiagOpt = PD_HTML;
    AnalysisPurgeOpt = PurgeStmt;
    IPAMode = BasicInlining;
    ShowCheckerHelp = 0;
    AnalyzeAll = 0;
    AnalyzerDisplayProgress = 0;
    AnalyzeNestedBlocks = 0;
    EagerlyAssume = 0;
    TrimGraph = 0;
    VisualizeEGDot = 0;
    VisualizeEGUbi = 0;
    UnoptimizedCFG = 0;
    CFGAddImplicitDtors = 0;
    eagerlyTrimExplodedGraph = 0;
    PrintStats = 0;
    NoRetryExhausted = 0;
    // Cap the stack depth at 4 calls (5 stack frames, base + 4 calls).
    InlineMaxStackDepth = 5;
    InlineMaxFunctionSize = 200;
    InliningMode = NoRedundancy;
  }
};

}

#endif
