//===--- ASTReader.cpp - AST File Reader ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ASTReader class, which reads AST files.
//
//===----------------------------------------------------------------------===//

#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTDeserializationListener.h"
#include "clang/Serialization/ModuleManager.h"
#include "clang/Serialization/SerializationDiagnostic.h"
#include "ASTCommon.h"
#include "ASTReaderInternals.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Scope.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLocVisitor.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/PreprocessingRecord.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Basic/OnDiskHashTable.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/SourceManagerInternals.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/FileSystemStatCache.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/Version.h"
#include "clang/Basic/VersionTuple.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/system_error.h"
#include <algorithm>
#include <iterator>
#include <cstdio>
#include <sys/stat.h>

using namespace clang;
using namespace clang::serialization;
using namespace clang::serialization::reader;

//===----------------------------------------------------------------------===//
// PCH validator implementation
//===----------------------------------------------------------------------===//

ASTReaderListener::~ASTReaderListener() {}

/// \brief Compare the given set of language options against an existing set of
/// language options.
///
/// \param Diags If non-NULL, diagnostics will be emitted via this engine.
///
/// \returns true if the languagae options mis-match, false otherwise.
static bool checkLanguageOptions(const LangOptions &LangOpts,
                                 const LangOptions &ExistingLangOpts,
                                 DiagnosticsEngine *Diags) {
#define LANGOPT(Name, Bits, Default, Description)                 \
  if (ExistingLangOpts.Name != LangOpts.Name) {                   \
    if (Diags)                                                    \
      Diags->Report(diag::err_pch_langopt_mismatch)               \
        << Description << LangOpts.Name << ExistingLangOpts.Name; \
    return true;                                                  \
  }

#define VALUE_LANGOPT(Name, Bits, Default, Description)   \
  if (ExistingLangOpts.Name != LangOpts.Name) {           \
    if (Diags)                                            \
      Diags->Report(diag::err_pch_langopt_value_mismatch) \
        << Description;                                   \
    return true;                                          \
  }

#define ENUM_LANGOPT(Name, Type, Bits, Default, Description)   \
  if (ExistingLangOpts.get##Name() != LangOpts.get##Name()) {  \
    if (Diags)                                                 \
      Diags->Report(diag::err_pch_langopt_value_mismatch)      \
        << Description;                                        \
    return true;                                               \
  }

#define BENIGN_LANGOPT(Name, Bits, Default, Description)
#define BENIGN_ENUM_LANGOPT(Name, Type, Bits, Default, Description)
#include "clang/Basic/LangOptions.def"

  if (ExistingLangOpts.ObjCRuntime != LangOpts.ObjCRuntime) {
    if (Diags)
      Diags->Report(diag::err_pch_langopt_value_mismatch)
      << "target Objective-C runtime";
    return true;
  }

  return false;
}

/// \brief Compare the given set of target options against an existing set of
/// target options.
///
/// \param Diags If non-NULL, diagnostics will be emitted via this engine.
///
/// \returns true if the target options mis-match, false otherwise.
static bool checkTargetOptions(const TargetOptions &TargetOpts,
                               const TargetOptions &ExistingTargetOpts,
                               DiagnosticsEngine *Diags) {
#define CHECK_TARGET_OPT(Field, Name)                             \
  if (TargetOpts.Field != ExistingTargetOpts.Field) {             \
    if (Diags)                                                    \
      Diags->Report(diag::err_pch_targetopt_mismatch)             \
        << Name << TargetOpts.Field << ExistingTargetOpts.Field;  \
    return true;                                                  \
  }

  CHECK_TARGET_OPT(Triple, "target");
  CHECK_TARGET_OPT(CPU, "target CPU");
  CHECK_TARGET_OPT(ABI, "target ABI");
  CHECK_TARGET_OPT(CXXABI, "target C++ ABI");
  CHECK_TARGET_OPT(LinkerVersion, "target linker version");
#undef CHECK_TARGET_OPT

  // Compare feature sets.
  SmallVector<StringRef, 4> ExistingFeatures(
                                             ExistingTargetOpts.FeaturesAsWritten.begin(),
                                             ExistingTargetOpts.FeaturesAsWritten.end());
  SmallVector<StringRef, 4> ReadFeatures(TargetOpts.FeaturesAsWritten.begin(),
                                         TargetOpts.FeaturesAsWritten.end());
  std::sort(ExistingFeatures.begin(), ExistingFeatures.end());
  std::sort(ReadFeatures.begin(), ReadFeatures.end());

  unsigned ExistingIdx = 0, ExistingN = ExistingFeatures.size();
  unsigned ReadIdx = 0, ReadN = ReadFeatures.size();
  while (ExistingIdx < ExistingN && ReadIdx < ReadN) {
    if (ExistingFeatures[ExistingIdx] == ReadFeatures[ReadIdx]) {
      ++ExistingIdx;
      ++ReadIdx;
      continue;
    }

    if (ReadFeatures[ReadIdx] < ExistingFeatures[ExistingIdx]) {
      if (Diags)
        Diags->Report(diag::err_pch_targetopt_feature_mismatch)
          << false << ReadFeatures[ReadIdx];
      return true;
    }

    if (Diags)
      Diags->Report(diag::err_pch_targetopt_feature_mismatch)
        << true << ExistingFeatures[ExistingIdx];
    return true;
  }

  if (ExistingIdx < ExistingN) {
    if (Diags)
      Diags->Report(diag::err_pch_targetopt_feature_mismatch)
        << true << ExistingFeatures[ExistingIdx];
    return true;
  }

  if (ReadIdx < ReadN) {
    if (Diags)
      Diags->Report(diag::err_pch_targetopt_feature_mismatch)
        << false << ReadFeatures[ReadIdx];
    return true;
  }

  return false;
}

bool
PCHValidator::ReadLanguageOptions(const LangOptions &LangOpts,
                                  bool Complain) {
  const LangOptions &ExistingLangOpts = PP.getLangOpts();
  return checkLanguageOptions(LangOpts, ExistingLangOpts,
                              Complain? &Reader.Diags : 0);
}

bool PCHValidator::ReadTargetOptions(const TargetOptions &TargetOpts,
                                     bool Complain) {
  const TargetOptions &ExistingTargetOpts = PP.getTargetInfo().getTargetOpts();
  return checkTargetOptions(TargetOpts, ExistingTargetOpts,
                            Complain? &Reader.Diags : 0);
}

namespace {
  typedef llvm::StringMap<std::pair<StringRef, bool /*IsUndef*/> >
    MacroDefinitionsMap;
}

/// \brief Collect the macro definitions provided by the given preprocessor
/// options.
static void collectMacroDefinitions(const PreprocessorOptions &PPOpts,
                                    MacroDefinitionsMap &Macros,
                                    SmallVectorImpl<StringRef> *MacroNames = 0){
  for (unsigned I = 0, N = PPOpts.Macros.size(); I != N; ++I) {
    StringRef Macro = PPOpts.Macros[I].first;
    bool IsUndef = PPOpts.Macros[I].second;

    std::pair<StringRef, StringRef> MacroPair = Macro.split('=');
    StringRef MacroName = MacroPair.first;
    StringRef MacroBody = MacroPair.second;

    // For an #undef'd macro, we only care about the name.
    if (IsUndef) {
      if (MacroNames && !Macros.count(MacroName))
        MacroNames->push_back(MacroName);

      Macros[MacroName] = std::make_pair("", true);
      continue;
    }

    // For a #define'd macro, figure out the actual definition.
    if (MacroName.size() == Macro.size())
      MacroBody = "1";
    else {
      // Note: GCC drops anything following an end-of-line character.
      StringRef::size_type End = MacroBody.find_first_of("\n\r");
      MacroBody = MacroBody.substr(0, End);
    }

    if (MacroNames && !Macros.count(MacroName))
      MacroNames->push_back(MacroName);
    Macros[MacroName] = std::make_pair(MacroBody, false);
  }
}
         
/// \brief Check the preprocessor options deserialized from the control block
/// against the preprocessor options in an existing preprocessor.
///
/// \param Diags If non-null, produce diagnostics for any mismatches incurred.
static bool checkPreprocessorOptions(const PreprocessorOptions &PPOpts,
                                     const PreprocessorOptions &ExistingPPOpts,
                                     DiagnosticsEngine *Diags,
                                     FileManager &FileMgr,
                                     std::string &SuggestedPredefines) {
  // Check macro definitions.
  MacroDefinitionsMap ASTFileMacros;
  collectMacroDefinitions(PPOpts, ASTFileMacros);
  MacroDefinitionsMap ExistingMacros;
  SmallVector<StringRef, 4> ExistingMacroNames;
  collectMacroDefinitions(ExistingPPOpts, ExistingMacros, &ExistingMacroNames);

  for (unsigned I = 0, N = ExistingMacroNames.size(); I != N; ++I) {
    // Dig out the macro definition in the existing preprocessor options.
    StringRef MacroName = ExistingMacroNames[I];
    std::pair<StringRef, bool> Existing = ExistingMacros[MacroName];

    // Check whether we know anything about this macro name or not.
    llvm::StringMap<std::pair<StringRef, bool /*IsUndef*/> >::iterator Known
      = ASTFileMacros.find(MacroName);
    if (Known == ASTFileMacros.end()) {
      // FIXME: Check whether this identifier was referenced anywhere in the
      // AST file. If so, we should reject the AST file. Unfortunately, this
      // information isn't in the control block. What shall we do about it?

      if (Existing.second) {
        SuggestedPredefines += "#undef ";
        SuggestedPredefines += MacroName.str();
        SuggestedPredefines += '\n';
      } else {
        SuggestedPredefines += "#define ";
        SuggestedPredefines += MacroName.str();
        SuggestedPredefines += ' ';
        SuggestedPredefines += Existing.first.str();
        SuggestedPredefines += '\n';
      }
      continue;
    }

    // If the macro was defined in one but undef'd in the other, we have a
    // conflict.
    if (Existing.second != Known->second.second) {
      if (Diags) {
        Diags->Report(diag::err_pch_macro_def_undef)
          << MacroName << Known->second.second;
      }
      return true;
    }

    // If the macro was #undef'd in both, or if the macro bodies are identical,
    // it's fine.
    if (Existing.second || Existing.first == Known->second.first)
      continue;

    // The macro bodies differ; complain.
    if (Diags) {
      Diags->Report(diag::err_pch_macro_def_conflict)
        << MacroName << Known->second.first << Existing.first;
    }
    return true;
  }

  // Check whether we're using predefines.
  if (PPOpts.UsePredefines != ExistingPPOpts.UsePredefines) {
    if (Diags) {
      Diags->Report(diag::err_pch_undef) << ExistingPPOpts.UsePredefines;
    }
    return true;
  }

  // Compute the #include and #include_macros lines we need.
  for (unsigned I = 0, N = ExistingPPOpts.Includes.size(); I != N; ++I) {
    StringRef File = ExistingPPOpts.Includes[I];
    if (File == ExistingPPOpts.ImplicitPCHInclude)
      continue;

    if (std::find(PPOpts.Includes.begin(), PPOpts.Includes.end(), File)
          != PPOpts.Includes.end())
      continue;

    SuggestedPredefines += "#include \"";
    SuggestedPredefines +=
      HeaderSearch::NormalizeDashIncludePath(File, FileMgr);
    SuggestedPredefines += "\"\n";
  }

  for (unsigned I = 0, N = ExistingPPOpts.MacroIncludes.size(); I != N; ++I) {
    StringRef File = ExistingPPOpts.MacroIncludes[I];
    if (std::find(PPOpts.MacroIncludes.begin(), PPOpts.MacroIncludes.end(),
                  File)
        != PPOpts.MacroIncludes.end())
      continue;

    SuggestedPredefines += "#__include_macros \"";
    SuggestedPredefines +=
      HeaderSearch::NormalizeDashIncludePath(File, FileMgr);
    SuggestedPredefines += "\"\n##\n";
  }

  return false;
}

bool PCHValidator::ReadPreprocessorOptions(const PreprocessorOptions &PPOpts,
                                           bool Complain,
                                           std::string &SuggestedPredefines) {
  const PreprocessorOptions &ExistingPPOpts = PP.getPreprocessorOpts();

  return checkPreprocessorOptions(PPOpts, ExistingPPOpts,
                                  Complain? &Reader.Diags : 0,
                                  PP.getFileManager(),
                                  SuggestedPredefines);
}

void PCHValidator::ReadHeaderFileInfo(const HeaderFileInfo &HFI,
                                      unsigned ID) {
  PP.getHeaderSearchInfo().setHeaderFileInfoForUID(HFI, ID);
  ++NumHeaderInfos;
}

void PCHValidator::ReadCounter(const ModuleFile &M, unsigned Value) {
  PP.setCounterValue(Value);
}

//===----------------------------------------------------------------------===//
// AST reader implementation
//===----------------------------------------------------------------------===//

void
ASTReader::setDeserializationListener(ASTDeserializationListener *Listener) {
  DeserializationListener = Listener;
}



unsigned ASTSelectorLookupTrait::ComputeHash(Selector Sel) {
  return serialization::ComputeHash(Sel);
}


std::pair<unsigned, unsigned>
ASTSelectorLookupTrait::ReadKeyDataLength(const unsigned char*& d) {
  using namespace clang::io;
  unsigned KeyLen = ReadUnalignedLE16(d);
  unsigned DataLen = ReadUnalignedLE16(d);
  return std::make_pair(KeyLen, DataLen);
}

ASTSelectorLookupTrait::internal_key_type 
ASTSelectorLookupTrait::ReadKey(const unsigned char* d, unsigned) {
  using namespace clang::io;
  SelectorTable &SelTable = Reader.getContext().Selectors;
  unsigned N = ReadUnalignedLE16(d);
  IdentifierInfo *FirstII
    = Reader.getLocalIdentifier(F, ReadUnalignedLE32(d));
  if (N == 0)
    return SelTable.getNullarySelector(FirstII);
  else if (N == 1)
    return SelTable.getUnarySelector(FirstII);

  SmallVector<IdentifierInfo *, 16> Args;
  Args.push_back(FirstII);
  for (unsigned I = 1; I != N; ++I)
    Args.push_back(Reader.getLocalIdentifier(F, ReadUnalignedLE32(d)));

  return SelTable.getSelector(N, Args.data());
}

ASTSelectorLookupTrait::data_type 
ASTSelectorLookupTrait::ReadData(Selector, const unsigned char* d, 
                                 unsigned DataLen) {
  using namespace clang::io;

  data_type Result;

  Result.ID = Reader.getGlobalSelectorID(F, ReadUnalignedLE32(d));
  unsigned NumInstanceMethods = ReadUnalignedLE16(d);
  unsigned NumFactoryMethods = ReadUnalignedLE16(d);

  // Load instance methods
  for (unsigned I = 0; I != NumInstanceMethods; ++I) {
    if (ObjCMethodDecl *Method
          = Reader.GetLocalDeclAs<ObjCMethodDecl>(F, ReadUnalignedLE32(d)))
      Result.Instance.push_back(Method);
  }

  // Load factory methods
  for (unsigned I = 0; I != NumFactoryMethods; ++I) {
    if (ObjCMethodDecl *Method
          = Reader.GetLocalDeclAs<ObjCMethodDecl>(F, ReadUnalignedLE32(d)))
      Result.Factory.push_back(Method);
  }

  return Result;
}

unsigned ASTIdentifierLookupTrait::ComputeHash(const internal_key_type& a) {
  return llvm::HashString(StringRef(a.first, a.second));
}

std::pair<unsigned, unsigned>
ASTIdentifierLookupTrait::ReadKeyDataLength(const unsigned char*& d) {
  using namespace clang::io;
  unsigned DataLen = ReadUnalignedLE16(d);
  unsigned KeyLen = ReadUnalignedLE16(d);
  return std::make_pair(KeyLen, DataLen);
}

std::pair<const char*, unsigned>
ASTIdentifierLookupTrait::ReadKey(const unsigned char* d, unsigned n) {
  assert(n >= 2 && d[n-1] == '\0');
  return std::make_pair((const char*) d, n-1);
}

IdentifierInfo *ASTIdentifierLookupTrait::ReadData(const internal_key_type& k,
                                                   const unsigned char* d,
                                                   unsigned DataLen) {
  using namespace clang::io;
  unsigned RawID = ReadUnalignedLE32(d);
  bool IsInteresting = RawID & 0x01;

  // Wipe out the "is interesting" bit.
  RawID = RawID >> 1;

  IdentID ID = Reader.getGlobalIdentifierID(F, RawID);
  if (!IsInteresting) {
    // For uninteresting identifiers, just build the IdentifierInfo
    // and associate it with the persistent ID.
    IdentifierInfo *II = KnownII;
    if (!II) {
      II = &Reader.getIdentifierTable().getOwn(StringRef(k.first, k.second));
      KnownII = II;
    }
    Reader.SetIdentifierInfo(ID, II);
    II->setIsFromAST();
    Reader.markIdentifierUpToDate(II);    
    return II;
  }

  unsigned ObjCOrBuiltinID = ReadUnalignedLE16(d);
  unsigned Bits = ReadUnalignedLE16(d);
  bool CPlusPlusOperatorKeyword = Bits & 0x01;
  Bits >>= 1;
  bool HasRevertedTokenIDToIdentifier = Bits & 0x01;
  Bits >>= 1;
  bool Poisoned = Bits & 0x01;
  Bits >>= 1;
  bool ExtensionToken = Bits & 0x01;
  Bits >>= 1;
  bool hadMacroDefinition = Bits & 0x01;
  Bits >>= 1;

  assert(Bits == 0 && "Extra bits in the identifier?");
  DataLen -= 8;

  // Build the IdentifierInfo itself and link the identifier ID with
  // the new IdentifierInfo.
  IdentifierInfo *II = KnownII;
  if (!II) {
    II = &Reader.getIdentifierTable().getOwn(StringRef(k.first, k.second));
    KnownII = II;
  }
  Reader.markIdentifierUpToDate(II);
  II->setIsFromAST();

  // Set or check the various bits in the IdentifierInfo structure.
  // Token IDs are read-only.
  if (HasRevertedTokenIDToIdentifier)
    II->RevertTokenIDToIdentifier();
  II->setObjCOrBuiltinID(ObjCOrBuiltinID);
  assert(II->isExtensionToken() == ExtensionToken &&
         "Incorrect extension token flag");
  (void)ExtensionToken;
  if (Poisoned)
    II->setIsPoisoned(true);
  assert(II->isCPlusPlusOperatorKeyword() == CPlusPlusOperatorKeyword &&
         "Incorrect C++ operator keyword flag");
  (void)CPlusPlusOperatorKeyword;

  // If this identifier is a macro, deserialize the macro
  // definition.
  if (hadMacroDefinition) {
    SmallVector<MacroID, 4> MacroIDs;
    while (uint32_t LocalID = ReadUnalignedLE32(d)) {
      MacroIDs.push_back(Reader.getGlobalMacroID(F, LocalID));
      DataLen -= 4;
    }
    DataLen -= 4;
    Reader.setIdentifierIsMacro(II, MacroIDs);
  }

  Reader.SetIdentifierInfo(ID, II);

  // Read all of the declarations visible at global scope with this
  // name.
  if (DataLen > 0) {
    SmallVector<uint32_t, 4> DeclIDs;
    for (; DataLen > 0; DataLen -= 4)
      DeclIDs.push_back(Reader.getGlobalDeclID(F, ReadUnalignedLE32(d)));
    Reader.SetGloballyVisibleDecls(II, DeclIDs);
  }

  return II;
}

unsigned 
ASTDeclContextNameLookupTrait::ComputeHash(const DeclNameKey &Key) const {
  llvm::FoldingSetNodeID ID;
  ID.AddInteger(Key.Kind);

  switch (Key.Kind) {
  case DeclarationName::Identifier:
  case DeclarationName::CXXLiteralOperatorName:
    ID.AddString(((IdentifierInfo*)Key.Data)->getName());
    break;
  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    ID.AddInteger(serialization::ComputeHash(Selector(Key.Data)));
    break;
  case DeclarationName::CXXOperatorName:
    ID.AddInteger((OverloadedOperatorKind)Key.Data);
    break;
  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName:
  case DeclarationName::CXXUsingDirective:
    break;
  }

  return ID.ComputeHash();
}

ASTDeclContextNameLookupTrait::internal_key_type 
ASTDeclContextNameLookupTrait::GetInternalKey(
                                          const external_key_type& Name) const {
  DeclNameKey Key;
  Key.Kind = Name.getNameKind();
  switch (Name.getNameKind()) {
  case DeclarationName::Identifier:
    Key.Data = (uint64_t)Name.getAsIdentifierInfo();
    break;
  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    Key.Data = (uint64_t)Name.getObjCSelector().getAsOpaquePtr();
    break;
  case DeclarationName::CXXOperatorName:
    Key.Data = Name.getCXXOverloadedOperator();
    break;
  case DeclarationName::CXXLiteralOperatorName:
    Key.Data = (uint64_t)Name.getCXXLiteralIdentifier();
    break;
  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName:
  case DeclarationName::CXXUsingDirective:
    Key.Data = 0;
    break;
  }

  return Key;
}

std::pair<unsigned, unsigned>
ASTDeclContextNameLookupTrait::ReadKeyDataLength(const unsigned char*& d) {
  using namespace clang::io;
  unsigned KeyLen = ReadUnalignedLE16(d);
  unsigned DataLen = ReadUnalignedLE16(d);
  return std::make_pair(KeyLen, DataLen);
}

ASTDeclContextNameLookupTrait::internal_key_type 
ASTDeclContextNameLookupTrait::ReadKey(const unsigned char* d, unsigned) {
  using namespace clang::io;

  DeclNameKey Key;
  Key.Kind = (DeclarationName::NameKind)*d++;
  switch (Key.Kind) {
  case DeclarationName::Identifier:
    Key.Data = (uint64_t)Reader.getLocalIdentifier(F, ReadUnalignedLE32(d));
    break;
  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    Key.Data =
       (uint64_t)Reader.getLocalSelector(F, ReadUnalignedLE32(d))
                   .getAsOpaquePtr();
    break;
  case DeclarationName::CXXOperatorName:
    Key.Data = *d++; // OverloadedOperatorKind
    break;
  case DeclarationName::CXXLiteralOperatorName:
    Key.Data = (uint64_t)Reader.getLocalIdentifier(F, ReadUnalignedLE32(d));
    break;
  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName:
  case DeclarationName::CXXUsingDirective:
    Key.Data = 0;
    break;
  }

  return Key;
}

ASTDeclContextNameLookupTrait::data_type 
ASTDeclContextNameLookupTrait::ReadData(internal_key_type, 
                                        const unsigned char* d,
                                        unsigned DataLen) {
  using namespace clang::io;
  unsigned NumDecls = ReadUnalignedLE16(d);
  LE32DeclID *Start = (LE32DeclID *)d;
  return std::make_pair(Start, Start + NumDecls);
}

bool ASTReader::ReadDeclContextStorage(ModuleFile &M,
                                       llvm::BitstreamCursor &Cursor,
                                   const std::pair<uint64_t, uint64_t> &Offsets,
                                       DeclContextInfo &Info) {
  SavedStreamPosition SavedPosition(Cursor);
  // First the lexical decls.
  if (Offsets.first != 0) {
    Cursor.JumpToBit(Offsets.first);

    RecordData Record;
    const char *Blob;
    unsigned BlobLen;
    unsigned Code = Cursor.ReadCode();
    unsigned RecCode = Cursor.ReadRecord(Code, Record, &Blob, &BlobLen);
    if (RecCode != DECL_CONTEXT_LEXICAL) {
      Error("Expected lexical block");
      return true;
    }

    Info.LexicalDecls = reinterpret_cast<const KindDeclIDPair*>(Blob);
    Info.NumLexicalDecls = BlobLen / sizeof(KindDeclIDPair);
  }

  // Now the lookup table.
  if (Offsets.second != 0) {
    Cursor.JumpToBit(Offsets.second);

    RecordData Record;
    const char *Blob;
    unsigned BlobLen;
    unsigned Code = Cursor.ReadCode();
    unsigned RecCode = Cursor.ReadRecord(Code, Record, &Blob, &BlobLen);
    if (RecCode != DECL_CONTEXT_VISIBLE) {
      Error("Expected visible lookup table block");
      return true;
    }
    Info.NameLookupTableData
      = ASTDeclContextNameLookupTable::Create(
                    (const unsigned char *)Blob + Record[0],
                    (const unsigned char *)Blob,
                    ASTDeclContextNameLookupTrait(*this, M));
  }

  return false;
}

void ASTReader::Error(StringRef Msg) {
  Error(diag::err_fe_pch_malformed, Msg);
}

void ASTReader::Error(unsigned DiagID,
                      StringRef Arg1, StringRef Arg2) {
  if (Diags.isDiagnosticInFlight())
    Diags.SetDelayedDiagnostic(DiagID, Arg1, Arg2);
  else
    Diag(DiagID) << Arg1 << Arg2;
}

//===----------------------------------------------------------------------===//
// Source Manager Deserialization
//===----------------------------------------------------------------------===//

/// \brief Read the line table in the source manager block.
/// \returns true if there was an error.
bool ASTReader::ParseLineTable(ModuleFile &F,
                               SmallVectorImpl<uint64_t> &Record) {
  unsigned Idx = 0;
  LineTableInfo &LineTable = SourceMgr.getLineTable();

  // Parse the file names
  std::map<int, int> FileIDs;
  for (int I = 0, N = Record[Idx++]; I != N; ++I) {
    // Extract the file name
    unsigned FilenameLen = Record[Idx++];
    std::string Filename(&Record[Idx], &Record[Idx] + FilenameLen);
    Idx += FilenameLen;
    MaybeAddSystemRootToFilename(F, Filename);
    FileIDs[I] = LineTable.getLineTableFilenameID(Filename);
  }

  // Parse the line entries
  std::vector<LineEntry> Entries;
  while (Idx < Record.size()) {
    int FID = Record[Idx++];
    assert(FID >= 0 && "Serialized line entries for non-local file.");
    // Remap FileID from 1-based old view.
    FID += F.SLocEntryBaseID - 1;

    // Extract the line entries
    unsigned NumEntries = Record[Idx++];
    assert(NumEntries && "Numentries is 00000");
    Entries.clear();
    Entries.reserve(NumEntries);
    for (unsigned I = 0; I != NumEntries; ++I) {
      unsigned FileOffset = Record[Idx++];
      unsigned LineNo = Record[Idx++];
      int FilenameID = FileIDs[Record[Idx++]];
      SrcMgr::CharacteristicKind FileKind
        = (SrcMgr::CharacteristicKind)Record[Idx++];
      unsigned IncludeOffset = Record[Idx++];
      Entries.push_back(LineEntry::get(FileOffset, LineNo, FilenameID,
                                       FileKind, IncludeOffset));
    }
    LineTable.AddEntry(FileID::get(FID), Entries);
  }

  return false;
}

/// \brief Read a source manager block
bool ASTReader::ReadSourceManagerBlock(ModuleFile &F) {
  using namespace SrcMgr;

  llvm::BitstreamCursor &SLocEntryCursor = F.SLocEntryCursor;

  // Set the source-location entry cursor to the current position in
  // the stream. This cursor will be used to read the contents of the
  // source manager block initially, and then lazily read
  // source-location entries as needed.
  SLocEntryCursor = F.Stream;

  // The stream itself is going to skip over the source manager block.
  if (F.Stream.SkipBlock()) {
    Error("malformed block record in AST file");
    return true;
  }

  // Enter the source manager block.
  if (SLocEntryCursor.EnterSubBlock(SOURCE_MANAGER_BLOCK_ID)) {
    Error("malformed source manager block record in AST file");
    return true;
  }

  RecordData Record;
  while (true) {
    unsigned Code = SLocEntryCursor.ReadCode();
    if (Code == llvm::bitc::END_BLOCK) {
      if (SLocEntryCursor.ReadBlockEnd()) {
        Error("error at end of Source Manager block in AST file");
        return true;
      }
      return false;
    }

    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      // No known subblocks, always skip them.
      SLocEntryCursor.ReadSubBlockID();
      if (SLocEntryCursor.SkipBlock()) {
        Error("malformed block record in AST file");
        return true;
      }
      continue;
    }

    if (Code == llvm::bitc::DEFINE_ABBREV) {
      SLocEntryCursor.ReadAbbrevRecord();
      continue;
    }

    // Read a record.
    const char *BlobStart;
    unsigned BlobLen;
    Record.clear();
    switch (SLocEntryCursor.ReadRecord(Code, Record, &BlobStart, &BlobLen)) {
    default:  // Default behavior: ignore.
      break;

    case SM_SLOC_FILE_ENTRY:
    case SM_SLOC_BUFFER_ENTRY:
    case SM_SLOC_EXPANSION_ENTRY:
      // Once we hit one of the source location entries, we're done.
      return false;
    }
  }
}

/// \brief If a header file is not found at the path that we expect it to be
/// and the PCH file was moved from its original location, try to resolve the
/// file by assuming that header+PCH were moved together and the header is in
/// the same place relative to the PCH.
static std::string
resolveFileRelativeToOriginalDir(const std::string &Filename,
                                 const std::string &OriginalDir,
                                 const std::string &CurrDir) {
  assert(OriginalDir != CurrDir &&
         "No point trying to resolve the file if the PCH dir didn't change");
  using namespace llvm::sys;
  SmallString<128> filePath(Filename);
  fs::make_absolute(filePath);
  assert(path::is_absolute(OriginalDir));
  SmallString<128> currPCHPath(CurrDir);

  path::const_iterator fileDirI = path::begin(path::parent_path(filePath)),
                       fileDirE = path::end(path::parent_path(filePath));
  path::const_iterator origDirI = path::begin(OriginalDir),
                       origDirE = path::end(OriginalDir);
  // Skip the common path components from filePath and OriginalDir.
  while (fileDirI != fileDirE && origDirI != origDirE &&
         *fileDirI == *origDirI) {
    ++fileDirI;
    ++origDirI;
  }
  for (; origDirI != origDirE; ++origDirI)
    path::append(currPCHPath, "..");
  path::append(currPCHPath, fileDirI, fileDirE);
  path::append(currPCHPath, path::filename(Filename));
  return currPCHPath.str();
}

bool ASTReader::ReadSLocEntry(int ID) {
  if (ID == 0)
    return false;

  if (unsigned(-ID) - 2 >= getTotalNumSLocs() || ID > 0) {
    Error("source location entry ID out-of-range for AST file");
    return true;
  }

  ModuleFile *F = GlobalSLocEntryMap.find(-ID)->second;
  F->SLocEntryCursor.JumpToBit(F->SLocEntryOffsets[ID - F->SLocEntryBaseID]);
  llvm::BitstreamCursor &SLocEntryCursor = F->SLocEntryCursor;
  unsigned BaseOffset = F->SLocEntryBaseOffset;

  ++NumSLocEntriesRead;
  unsigned Code = SLocEntryCursor.ReadCode();
  if (Code == llvm::bitc::END_BLOCK ||
      Code == llvm::bitc::ENTER_SUBBLOCK ||
      Code == llvm::bitc::DEFINE_ABBREV) {
    Error("incorrectly-formatted source location entry in AST file");
    return true;
  }

  RecordData Record;
  const char *BlobStart;
  unsigned BlobLen;
  switch (SLocEntryCursor.ReadRecord(Code, Record, &BlobStart, &BlobLen)) {
  default:
    Error("incorrectly-formatted source location entry in AST file");
    return true;

  case SM_SLOC_FILE_ENTRY: {
    // We will detect whether a file changed and return 'Failure' for it, but
    // we will also try to fail gracefully by setting up the SLocEntry.
    unsigned InputID = Record[4];
    InputFile IF = getInputFile(*F, InputID);
    const FileEntry *File = IF.getPointer();
    bool OverriddenBuffer = IF.getInt();

    if (!IF.getPointer())
      return true;

    SourceLocation IncludeLoc = ReadSourceLocation(*F, Record[1]);
    if (IncludeLoc.isInvalid() && F->Kind != MK_MainFile) {
      // This is the module's main file.
      IncludeLoc = getImportLocation(F);
    }
    SrcMgr::CharacteristicKind
      FileCharacter = (SrcMgr::CharacteristicKind)Record[2];
    FileID FID = SourceMgr.createFileID(File, IncludeLoc, FileCharacter,
                                        ID, BaseOffset + Record[0]);
    SrcMgr::FileInfo &FileInfo =
          const_cast<SrcMgr::FileInfo&>(SourceMgr.getSLocEntry(FID).getFile());
    FileInfo.NumCreatedFIDs = Record[5];
    if (Record[3])
      FileInfo.setHasLineDirectives();

    const DeclID *FirstDecl = F->FileSortedDecls + Record[6];
    unsigned NumFileDecls = Record[7];
    if (NumFileDecls) {
      assert(F->FileSortedDecls && "FILE_SORTED_DECLS not encountered yet ?");
      FileDeclIDs[FID] = FileDeclsInfo(F, llvm::makeArrayRef(FirstDecl,
                                                             NumFileDecls));
    }
    
    const SrcMgr::ContentCache *ContentCache
      = SourceMgr.getOrCreateContentCache(File,
                              /*isSystemFile=*/FileCharacter != SrcMgr::C_User);
    if (OverriddenBuffer && !ContentCache->BufferOverridden &&
        ContentCache->ContentsEntry == ContentCache->OrigEntry) {
      unsigned Code = SLocEntryCursor.ReadCode();
      Record.clear();
      unsigned RecCode
        = SLocEntryCursor.ReadRecord(Code, Record, &BlobStart, &BlobLen);
      
      if (RecCode != SM_SLOC_BUFFER_BLOB) {
        Error("AST record has invalid code");
        return true;
      }
      
      llvm::MemoryBuffer *Buffer
        = llvm::MemoryBuffer::getMemBuffer(StringRef(BlobStart, BlobLen - 1),
                                           File->getName());
      SourceMgr.overrideFileContents(File, Buffer);
    }

    break;
  }

  case SM_SLOC_BUFFER_ENTRY: {
    const char *Name = BlobStart;
    unsigned Offset = Record[0];
    SourceLocation IncludeLoc = ReadSourceLocation(*F, Record[1]);
    unsigned Code = SLocEntryCursor.ReadCode();
    Record.clear();
    unsigned RecCode
      = SLocEntryCursor.ReadRecord(Code, Record, &BlobStart, &BlobLen);

    if (RecCode != SM_SLOC_BUFFER_BLOB) {
      Error("AST record has invalid code");
      return true;
    }

    llvm::MemoryBuffer *Buffer
      = llvm::MemoryBuffer::getMemBuffer(StringRef(BlobStart, BlobLen - 1),
                                         Name);
    SourceMgr.createFileIDForMemBuffer(Buffer, ID, BaseOffset + Offset,
                                       IncludeLoc);
    break;
  }

  case SM_SLOC_EXPANSION_ENTRY: {
    SourceLocation SpellingLoc = ReadSourceLocation(*F, Record[1]);
    SourceMgr.createExpansionLoc(SpellingLoc,
                                     ReadSourceLocation(*F, Record[2]),
                                     ReadSourceLocation(*F, Record[3]),
                                     Record[4],
                                     ID,
                                     BaseOffset + Record[0]);
    break;
  }
  }

  return false;
}

/// \brief Find the location where the module F is imported.
SourceLocation ASTReader::getImportLocation(ModuleFile *F) {
  if (F->ImportLoc.isValid())
    return F->ImportLoc;
  
  // Otherwise we have a PCH. It's considered to be "imported" at the first
  // location of its includer.
  if (F->ImportedBy.empty() || !F->ImportedBy[0]) {
    // Main file is the importer. We assume that it is the first entry in the
    // entry table. We can't ask the manager, because at the time of PCH loading
    // the main file entry doesn't exist yet.
    // The very first entry is the invalid instantiation loc, which takes up
    // offsets 0 and 1.
    return SourceLocation::getFromRawEncoding(2U);
  }
  //return F->Loaders[0]->FirstLoc;
  return F->ImportedBy[0]->FirstLoc;
}

/// ReadBlockAbbrevs - Enter a subblock of the specified BlockID with the
/// specified cursor.  Read the abbreviations that are at the top of the block
/// and then leave the cursor pointing into the block.
bool ASTReader::ReadBlockAbbrevs(llvm::BitstreamCursor &Cursor,
                                 unsigned BlockID) {
  if (Cursor.EnterSubBlock(BlockID)) {
    Error("malformed block record in AST file");
    return Failure;
  }

  while (true) {
    uint64_t Offset = Cursor.GetCurrentBitNo();
    unsigned Code = Cursor.ReadCode();

    // We expect all abbrevs to be at the start of the block.
    if (Code != llvm::bitc::DEFINE_ABBREV) {
      Cursor.JumpToBit(Offset);
      return false;
    }
    Cursor.ReadAbbrevRecord();
  }
}

void ASTReader::ReadMacroRecord(ModuleFile &F, uint64_t Offset,
                                MacroInfo *Hint) {
  llvm::BitstreamCursor &Stream = F.MacroCursor;

  // Keep track of where we are in the stream, then jump back there
  // after reading this macro.
  SavedStreamPosition SavedPosition(Stream);

  Stream.JumpToBit(Offset);
  RecordData Record;
  SmallVector<IdentifierInfo*, 16> MacroArgs;
  MacroInfo *Macro = 0;

  // RAII object to add the loaded macro information once we're done
  // adding tokens.
  struct AddLoadedMacroInfoRAII {
    Preprocessor &PP;
    MacroInfo *Hint;
    MacroInfo *MI;
    IdentifierInfo *II;

    AddLoadedMacroInfoRAII(Preprocessor &PP, MacroInfo *Hint)
      : PP(PP), Hint(Hint), MI(), II() { }
    ~AddLoadedMacroInfoRAII( ) {
      if (MI) {
        // Finally, install the macro.
        PP.addLoadedMacroInfo(II, MI, Hint);
      }
    }
  } AddLoadedMacroInfo(PP, Hint);

  while (true) {
    unsigned Code = Stream.ReadCode();
    switch (Code) {
    case llvm::bitc::END_BLOCK:
      return;

    case llvm::bitc::ENTER_SUBBLOCK:
      // No known subblocks, always skip them.
      Stream.ReadSubBlockID();
      if (Stream.SkipBlock()) {
        Error("malformed block record in AST file");
        return;
      }
      continue;

    case llvm::bitc::DEFINE_ABBREV:
      Stream.ReadAbbrevRecord();
      continue;
    default: break;
    }

    // Read a record.
    const char *BlobStart = 0;
    unsigned BlobLen = 0;
    Record.clear();
    PreprocessorRecordTypes RecType =
      (PreprocessorRecordTypes)Stream.ReadRecord(Code, Record, BlobStart,
                                                 BlobLen);
    switch (RecType) {
    case PP_MACRO_OBJECT_LIKE:
    case PP_MACRO_FUNCTION_LIKE: {
      // If we already have a macro, that means that we've hit the end
      // of the definition of the macro we were looking for. We're
      // done.
      if (Macro)
        return;

      IdentifierInfo *II = getLocalIdentifier(F, Record[0]);
      if (II == 0) {
        Error("macro must have a name in AST file");
        return;
      }

      unsigned GlobalID = getGlobalMacroID(F, Record[1]);

      // If this macro has already been loaded, don't do so again.
      if (MacrosLoaded[GlobalID - NUM_PREDEF_MACRO_IDS])
        return;

      SubmoduleID GlobalSubmoduleID = getGlobalSubmoduleID(F, Record[2]);
      unsigned NextIndex = 3;
      SourceLocation Loc = ReadSourceLocation(F, Record, NextIndex);
      MacroInfo *MI = PP.AllocateMacroInfo(Loc);

      // Record this macro.
      MacrosLoaded[GlobalID - NUM_PREDEF_MACRO_IDS] = MI;

      SourceLocation UndefLoc = ReadSourceLocation(F, Record, NextIndex);
      if (UndefLoc.isValid())
        MI->setUndefLoc(UndefLoc);

      MI->setIsUsed(Record[NextIndex++]);
      MI->setIsFromAST();

      bool IsPublic = Record[NextIndex++];
      MI->setVisibility(IsPublic, ReadSourceLocation(F, Record, NextIndex));

      if (RecType == PP_MACRO_FUNCTION_LIKE) {
        // Decode function-like macro info.
        bool isC99VarArgs = Record[NextIndex++];
        bool isGNUVarArgs = Record[NextIndex++];
        MacroArgs.clear();
        unsigned NumArgs = Record[NextIndex++];
        for (unsigned i = 0; i != NumArgs; ++i)
          MacroArgs.push_back(getLocalIdentifier(F, Record[NextIndex++]));

        // Install function-like macro info.
        MI->setIsFunctionLike();
        if (isC99VarArgs) MI->setIsC99Varargs();
        if (isGNUVarArgs) MI->setIsGNUVarargs();
        MI->setArgumentList(MacroArgs.data(), MacroArgs.size(),
                            PP.getPreprocessorAllocator());
      }

      if (DeserializationListener)
        DeserializationListener->MacroRead(GlobalID, MI);

      // If an update record marked this as undefined, do so now.
      // FIXME: Only if the submodule this update came from is visible?
      MacroUpdatesMap::iterator Update = MacroUpdates.find(GlobalID);
      if (Update != MacroUpdates.end()) {
        if (MI->getUndefLoc().isInvalid()) {
          for (unsigned I = 0, N = Update->second.size(); I != N; ++I) {
            bool Hidden = false;
            if (unsigned SubmoduleID = Update->second[I].first) {
              if (Module *Owner = getSubmodule(SubmoduleID)) {
                if (Owner->NameVisibility == Module::Hidden) {
                  // Note that this #undef is hidden.
                  Hidden = true;

                  // Record this hiding for later.
                  HiddenNamesMap[Owner].push_back(
                    HiddenName(II, MI, Update->second[I].second.UndefLoc));
                }
              }
            }

            if (!Hidden) {
              MI->setUndefLoc(Update->second[I].second.UndefLoc);
              if (PPMutationListener *Listener = PP.getPPMutationListener())
                Listener->UndefinedMacro(MI);
              break;
            }
          }
        }
        MacroUpdates.erase(Update);
      }

      // Determine whether this macro definition is visible.
      bool Hidden = !MI->isPublic();
      if (!Hidden && GlobalSubmoduleID) {
        if (Module *Owner = getSubmodule(GlobalSubmoduleID)) {
          if (Owner->NameVisibility == Module::Hidden) {
            // The owning module is not visible, and this macro definition
            // should not be, either.
            Hidden = true;

            // Note that this macro definition was hidden because its owning
            // module is not yet visible.
            HiddenNamesMap[Owner].push_back(HiddenName(II, MI));
          }
        }
      }
      MI->setHidden(Hidden);

      // Make sure we install the macro once we're done.
      AddLoadedMacroInfo.MI = MI;
      AddLoadedMacroInfo.II = II;

      // Remember that we saw this macro last so that we add the tokens that
      // form its body to it.
      Macro = MI;

      if (NextIndex + 1 == Record.size() && PP.getPreprocessingRecord() &&
          Record[NextIndex]) {
        // We have a macro definition. Register the association
        PreprocessedEntityID
            GlobalID = getGlobalPreprocessedEntityID(F, Record[NextIndex]);
        PreprocessingRecord &PPRec = *PP.getPreprocessingRecord();
        PPRec.RegisterMacroDefinition(Macro,
                            PPRec.getPPEntityID(GlobalID-1, /*isLoaded=*/true));
      }

      ++NumMacrosRead;
      break;
    }

    case PP_TOKEN: {
      // If we see a TOKEN before a PP_MACRO_*, then the file is
      // erroneous, just pretend we didn't see this.
      if (Macro == 0) break;

      Token Tok;
      Tok.startToken();
      Tok.setLocation(ReadSourceLocation(F, Record[0]));
      Tok.setLength(Record[1]);
      if (IdentifierInfo *II = getLocalIdentifier(F, Record[2]))
        Tok.setIdentifierInfo(II);
      Tok.setKind((tok::TokenKind)Record[3]);
      Tok.setFlag((Token::TokenFlags)Record[4]);
      Macro->AddTokenToBody(Tok);
      break;
    }
    }
  }
}

PreprocessedEntityID 
ASTReader::getGlobalPreprocessedEntityID(ModuleFile &M, unsigned LocalID) const {
  ContinuousRangeMap<uint32_t, int, 2>::const_iterator 
    I = M.PreprocessedEntityRemap.find(LocalID - NUM_PREDEF_PP_ENTITY_IDS);
  assert(I != M.PreprocessedEntityRemap.end() 
         && "Invalid index into preprocessed entity index remap");
  
  return LocalID + I->second;
}

unsigned HeaderFileInfoTrait::ComputeHash(const char *path) {
  return llvm::HashString(llvm::sys::path::filename(path));
}
    
HeaderFileInfoTrait::internal_key_type 
HeaderFileInfoTrait::GetInternalKey(const char *path) { return path; }
    
bool HeaderFileInfoTrait::EqualKey(internal_key_type a, internal_key_type b) {
  if (strcmp(a, b) == 0)
    return true;
  
  if (llvm::sys::path::filename(a) != llvm::sys::path::filename(b))
    return false;

  // Determine whether the actual files are equivalent.
  bool Result = false;
  if (llvm::sys::fs::equivalent(a, b, Result))
    return false;
  
  return Result;
}
    
std::pair<unsigned, unsigned>
HeaderFileInfoTrait::ReadKeyDataLength(const unsigned char*& d) {
  unsigned KeyLen = (unsigned) clang::io::ReadUnalignedLE16(d);
  unsigned DataLen = (unsigned) *d++;
  return std::make_pair(KeyLen + 1, DataLen);
}
    
HeaderFileInfoTrait::data_type 
HeaderFileInfoTrait::ReadData(const internal_key_type, const unsigned char *d,
                              unsigned DataLen) {
  const unsigned char *End = d + DataLen;
  using namespace clang::io;
  HeaderFileInfo HFI;
  unsigned Flags = *d++;
  HFI.isImport = (Flags >> 5) & 0x01;
  HFI.isPragmaOnce = (Flags >> 4) & 0x01;
  HFI.DirInfo = (Flags >> 2) & 0x03;
  HFI.Resolved = (Flags >> 1) & 0x01;
  HFI.IndexHeaderMapHeader = Flags & 0x01;
  HFI.NumIncludes = ReadUnalignedLE16(d);
  HFI.ControllingMacroID = Reader.getGlobalIdentifierID(M, 
                                                        ReadUnalignedLE32(d));
  if (unsigned FrameworkOffset = ReadUnalignedLE32(d)) {
    // The framework offset is 1 greater than the actual offset, 
    // since 0 is used as an indicator for "no framework name".
    StringRef FrameworkName(FrameworkStrings + FrameworkOffset - 1);
    HFI.Framework = HS->getUniqueFrameworkName(FrameworkName);
  }
  
  assert(End == d && "Wrong data length in HeaderFileInfo deserialization");
  (void)End;
        
  // This HeaderFileInfo was externally loaded.
  HFI.External = true;
  return HFI;
}

void ASTReader::setIdentifierIsMacro(IdentifierInfo *II, ArrayRef<MacroID> IDs){
  II->setHadMacroDefinition(true);
  assert(NumCurrentElementsDeserializing > 0 &&"Missing deserialization guard");
  PendingMacroIDs[II].append(IDs.begin(), IDs.end());
}

void ASTReader::ReadDefinedMacros() {
  // Note that we are loading defined macros.
  Deserializing Macros(this);

  for (ModuleReverseIterator I = ModuleMgr.rbegin(),
      E = ModuleMgr.rend(); I != E; ++I) {
    llvm::BitstreamCursor &MacroCursor = (*I)->MacroCursor;

    // If there was no preprocessor block, skip this file.
    if (!MacroCursor.getBitStreamReader())
      continue;

    llvm::BitstreamCursor Cursor = MacroCursor;
    Cursor.JumpToBit((*I)->MacroStartOffset);

    RecordData Record;
    while (true) {
      unsigned Code = Cursor.ReadCode();
      if (Code == llvm::bitc::END_BLOCK)
        break;

      if (Code == llvm::bitc::ENTER_SUBBLOCK) {
        // No known subblocks, always skip them.
        Cursor.ReadSubBlockID();
        if (Cursor.SkipBlock()) {
          Error("malformed block record in AST file");
          return;
        }
        continue;
      }

      if (Code == llvm::bitc::DEFINE_ABBREV) {
        Cursor.ReadAbbrevRecord();
        continue;
      }

      // Read a record.
      const char *BlobStart;
      unsigned BlobLen;
      Record.clear();
      switch (Cursor.ReadRecord(Code, Record, &BlobStart, &BlobLen)) {
      default:  // Default behavior: ignore.
        break;

      case PP_MACRO_OBJECT_LIKE:
      case PP_MACRO_FUNCTION_LIKE:
        getLocalIdentifier(**I, Record[0]);
        break;

      case PP_TOKEN:
        // Ignore tokens.
        break;
      }
    }
  }
}

namespace {
  /// \brief Visitor class used to look up identifirs in an AST file.
  class IdentifierLookupVisitor {
    StringRef Name;
    unsigned PriorGeneration;
    IdentifierInfo *Found;
  public:
    IdentifierLookupVisitor(StringRef Name, unsigned PriorGeneration) 
      : Name(Name), PriorGeneration(PriorGeneration), Found() { }
    
    static bool visit(ModuleFile &M, void *UserData) {
      IdentifierLookupVisitor *This
        = static_cast<IdentifierLookupVisitor *>(UserData);
      
      // If we've already searched this module file, skip it now.
      if (M.Generation <= This->PriorGeneration)
        return true;
      
      ASTIdentifierLookupTable *IdTable
        = (ASTIdentifierLookupTable *)M.IdentifierLookupTable;
      if (!IdTable)
        return false;
      
      ASTIdentifierLookupTrait Trait(IdTable->getInfoObj().getReader(),
                                     M, This->Found);
                                     
      std::pair<const char*, unsigned> Key(This->Name.begin(), 
                                           This->Name.size());
      ASTIdentifierLookupTable::iterator Pos = IdTable->find(Key, &Trait);
      if (Pos == IdTable->end())
        return false;
      
      // Dereferencing the iterator has the effect of building the
      // IdentifierInfo node and populating it with the various
      // declarations it needs.
      This->Found = *Pos;
      return true;
    }
    
    // \brief Retrieve the identifier info found within the module
    // files.
    IdentifierInfo *getIdentifierInfo() const { return Found; }
  };
}

void ASTReader::updateOutOfDateIdentifier(IdentifierInfo &II) {
  // Note that we are loading an identifier.
  Deserializing AnIdentifier(this);

  unsigned PriorGeneration = 0;
  if (getContext().getLangOpts().Modules)
    PriorGeneration = IdentifierGeneration[&II];
  
  IdentifierLookupVisitor Visitor(II.getName(), PriorGeneration);
  ModuleMgr.visit(IdentifierLookupVisitor::visit, &Visitor);
  markIdentifierUpToDate(&II);
}

void ASTReader::markIdentifierUpToDate(IdentifierInfo *II) {
  if (!II)
    return;
  
  II->setOutOfDate(false);

  // Update the generation for this identifier.
  if (getContext().getLangOpts().Modules)
    IdentifierGeneration[II] = CurrentGeneration;
}

llvm::PointerIntPair<const FileEntry *, 1, bool> 
ASTReader::getInputFile(ModuleFile &F, unsigned ID, bool Complain) {
  // If this ID is bogus, just return an empty input file.
  if (ID == 0 || ID > F.InputFilesLoaded.size())
    return InputFile();

  // If we've already loaded this input file, return it.
  if (F.InputFilesLoaded[ID-1].getPointer())
    return F.InputFilesLoaded[ID-1];

  // Go find this input file.
  llvm::BitstreamCursor &Cursor = F.InputFilesCursor;
  SavedStreamPosition SavedPosition(Cursor);
  Cursor.JumpToBit(F.InputFileOffsets[ID-1]);
  
  unsigned Code = Cursor.ReadCode();
  RecordData Record;
  const char *BlobStart = 0;
  unsigned BlobLen = 0;
  switch ((InputFileRecordTypes)Cursor.ReadRecord(Code, Record,
                                                  &BlobStart, &BlobLen)) {
  case INPUT_FILE: {
    unsigned StoredID = Record[0];
    assert(ID == StoredID && "Bogus stored ID or offset");
    (void)StoredID;
    off_t StoredSize = (off_t)Record[1];
    time_t StoredTime = (time_t)Record[2];
    bool Overridden = (bool)Record[3];
    
    // Get the file entry for this input file.
    StringRef OrigFilename(BlobStart, BlobLen);
    std::string Filename = OrigFilename;
    MaybeAddSystemRootToFilename(F, Filename);
    const FileEntry *File 
      = Overridden? FileMgr.getVirtualFile(Filename, StoredSize, StoredTime)
                  : FileMgr.getFile(Filename, /*OpenFile=*/false);
    
    // If we didn't find the file, resolve it relative to the
    // original directory from which this AST file was created.
    if (File == 0 && !F.OriginalDir.empty() && !CurrentDir.empty() &&
        F.OriginalDir != CurrentDir) {
      std::string Resolved = resolveFileRelativeToOriginalDir(Filename,
                                                              F.OriginalDir,
                                                              CurrentDir);
      if (!Resolved.empty())
        File = FileMgr.getFile(Resolved);
    }
    
    // For an overridden file, create a virtual file with the stored
    // size/timestamp.
    if (Overridden && File == 0) {
      File = FileMgr.getVirtualFile(Filename, StoredSize, StoredTime);
    }
    
    if (File == 0) {
      if (Complain) {
        std::string ErrorStr = "could not find file '";
        ErrorStr += Filename;
        ErrorStr += "' referenced by AST file";
        Error(ErrorStr.c_str());
      }
      return InputFile();
    }
    
    // Note that we've loaded this input file.
    F.InputFilesLoaded[ID-1] = InputFile(File, Overridden);
    
    // Check if there was a request to override the contents of the file
    // that was part of the precompiled header. Overridding such a file
    // can lead to problems when lexing using the source locations from the
    // PCH.
    SourceManager &SM = getSourceManager();
    if (!Overridden && SM.isFileOverridden(File)) {
      Error(diag::err_fe_pch_file_overridden, Filename);
      // After emitting the diagnostic, recover by disabling the override so
      // that the original file will be used.
      SM.disableFileContentsOverride(File);
      // The FileEntry is a virtual file entry with the size of the contents
      // that would override the original contents. Set it to the original's
      // size/time.
      FileMgr.modifyFileEntry(const_cast<FileEntry*>(File),
                              StoredSize, StoredTime);
    }

    // For an overridden file, there is nothing to validate.
    if (Overridden)
      return InputFile(File, Overridden);

    // The stat info from the FileEntry came from the cached stat
    // info of the PCH, so we cannot trust it.
    struct stat StatBuf;
    if (::stat(File->getName(), &StatBuf) != 0) {
      StatBuf.st_size = File->getSize();
      StatBuf.st_mtime = File->getModificationTime();
    }

    if ((StoredSize != StatBuf.st_size
#if !defined(LLVM_ON_WIN32)
         // In our regression testing, the Windows file system seems to
         // have inconsistent modification times that sometimes
         // erroneously trigger this error-handling path.
         || StoredTime != StatBuf.st_mtime
#endif
         )) {
      if (Complain)
        Error(diag::err_fe_pch_file_modified, Filename);
      
      return InputFile();
    }

    return InputFile(File, Overridden);
  }
  }

  return InputFile();
}

const FileEntry *ASTReader::getFileEntry(StringRef filenameStrRef) {
  ModuleFile &M = ModuleMgr.getPrimaryModule();
  std::string Filename = filenameStrRef;
  MaybeAddSystemRootToFilename(M, Filename);
  const FileEntry *File = FileMgr.getFile(Filename);
  if (File == 0 && !M.OriginalDir.empty() && !CurrentDir.empty() &&
      M.OriginalDir != CurrentDir) {
    std::string resolved = resolveFileRelativeToOriginalDir(Filename,
                                                            M.OriginalDir,
                                                            CurrentDir);
    if (!resolved.empty())
      File = FileMgr.getFile(resolved);
  }

  return File;
}

/// \brief If we are loading a relocatable PCH file, and the filename is
/// not an absolute path, add the system root to the beginning of the file
/// name.
void ASTReader::MaybeAddSystemRootToFilename(ModuleFile &M,
                                             std::string &Filename) {
  // If this is not a relocatable PCH file, there's nothing to do.
  if (!M.RelocatablePCH)
    return;

  if (Filename.empty() || llvm::sys::path::is_absolute(Filename))
    return;

  if (isysroot.empty()) {
    // If no system root was given, default to '/'
    Filename.insert(Filename.begin(), '/');
    return;
  }

  unsigned Length = isysroot.size();
  if (isysroot[Length - 1] != '/')
    Filename.insert(Filename.begin(), '/');

  Filename.insert(Filename.begin(), isysroot.begin(), isysroot.end());
}

ASTReader::ASTReadResult
ASTReader::ReadControlBlock(ModuleFile &F,
                            llvm::SmallVectorImpl<ModuleFile *> &Loaded,
                            unsigned ClientLoadCapabilities) {
  llvm::BitstreamCursor &Stream = F.Stream;

  if (Stream.EnterSubBlock(CONTROL_BLOCK_ID)) {
    Error("malformed block record in AST file");
    return Failure;
  }

  // Read all of the records and blocks in the control block.
  RecordData Record;
  while (!Stream.AtEndOfStream()) {
    unsigned Code = Stream.ReadCode();
    if (Code == llvm::bitc::END_BLOCK) {
      if (Stream.ReadBlockEnd()) {
        Error("error at end of control block in AST file");
        return Failure;
      }

      // Validate all of the input files.
      if (!DisableValidation) {
        bool Complain = (ClientLoadCapabilities & ARR_OutOfDate) == 0;
        for (unsigned I = 0, N = Record[0]; I < N; ++I)
          if (!getInputFile(F, I+1, Complain).getPointer())
            return OutOfDate;
      }

      return Success;
    }

    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      switch (Stream.ReadSubBlockID()) {
      case INPUT_FILES_BLOCK_ID:
        F.InputFilesCursor = Stream;
        if (Stream.SkipBlock() || // Skip with the main cursor
            // Read the abbreviations
            ReadBlockAbbrevs(F.InputFilesCursor, INPUT_FILES_BLOCK_ID)) {
          Error("malformed block record in AST file");
          return Failure;
        }
        continue;
        
      default:
        if (!Stream.SkipBlock())
          continue;
        break;
      }

      Error("malformed block record in AST file");
      return Failure;
    }

    if (Code == llvm::bitc::DEFINE_ABBREV) {
      Stream.ReadAbbrevRecord();
      continue;
    }

    // Read and process a record.
    Record.clear();
    const char *BlobStart = 0;
    unsigned BlobLen = 0;
    switch ((ControlRecordTypes)Stream.ReadRecord(Code, Record,
                                                  &BlobStart, &BlobLen)) {
    case METADATA: {
      if (Record[0] != VERSION_MAJOR && !DisableValidation) {
        if ((ClientLoadCapabilities & ARR_VersionMismatch) == 0)
          Diag(Record[0] < VERSION_MAJOR? diag::warn_pch_version_too_old
                                        : diag::warn_pch_version_too_new);
        return VersionMismatch;
      }

      bool hasErrors = Record[5];
      if (hasErrors && !DisableValidation && !AllowASTWithCompilerErrors) {
        Diag(diag::err_pch_with_compiler_errors);
        return HadErrors;
      }

      F.RelocatablePCH = Record[4];

      const std::string &CurBranch = getClangFullRepositoryVersion();
      StringRef ASTBranch(BlobStart, BlobLen);
      if (StringRef(CurBranch) != ASTBranch && !DisableValidation) {
        if ((ClientLoadCapabilities & ARR_VersionMismatch) == 0)
          Diag(diag::warn_pch_different_branch) << ASTBranch << CurBranch;
        return VersionMismatch;
      }
      break;
    }

    case IMPORTS: {
      // Load each of the imported PCH files. 
      unsigned Idx = 0, N = Record.size();
      while (Idx < N) {
        // Read information about the AST file.
        ModuleKind ImportedKind = (ModuleKind)Record[Idx++];
        unsigned Length = Record[Idx++];
        SmallString<128> ImportedFile(Record.begin() + Idx,
                                      Record.begin() + Idx + Length);
        Idx += Length;

        // Load the AST file.
        switch(ReadASTCore(ImportedFile, ImportedKind, &F, Loaded,
                           ClientLoadCapabilities)) {
        case Failure: return Failure;
          // If we have to ignore the dependency, we'll have to ignore this too.
        case OutOfDate: return OutOfDate;
        case VersionMismatch: return VersionMismatch;
        case ConfigurationMismatch: return ConfigurationMismatch;
        case HadErrors: return HadErrors;
        case Success: break;
        }
      }
      break;
    }

    case LANGUAGE_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch) == 0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParseLanguageOptions(Record, Complain, *Listener) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case TARGET_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch)==0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParseTargetOptions(Record, Complain, *Listener) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case DIAGNOSTIC_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch)==0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParseDiagnosticOptions(Record, Complain, *Listener) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case FILE_SYSTEM_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch)==0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParseFileSystemOptions(Record, Complain, *Listener) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case HEADER_SEARCH_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch)==0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParseHeaderSearchOptions(Record, Complain, *Listener) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case PREPROCESSOR_OPTIONS: {
      bool Complain = (ClientLoadCapabilities & ARR_ConfigurationMismatch)==0;
      if (Listener && &F == *ModuleMgr.begin() &&
          ParsePreprocessorOptions(Record, Complain, *Listener,
                                   SuggestedPredefines) &&
          !DisableValidation)
        return ConfigurationMismatch;
      break;
    }

    case ORIGINAL_FILE:
      F.OriginalSourceFileID = FileID::get(Record[0]);
      F.ActualOriginalSourceFileName.assign(BlobStart, BlobLen);
      F.OriginalSourceFileName = F.ActualOriginalSourceFileName;
      MaybeAddSystemRootToFilename(F, F.OriginalSourceFileName);
      break;

    case ORIGINAL_PCH_DIR:
      F.OriginalDir.assign(BlobStart, BlobLen);
      break;

    case INPUT_FILE_OFFSETS:
      F.InputFileOffsets = (const uint32_t *)BlobStart;
      F.InputFilesLoaded.resize(Record[0]);
      break;
    }
  }

  Error("premature end of bitstream in AST file");
  return Failure;
}

bool ASTReader::ReadASTBlock(ModuleFile &F) {
  llvm::BitstreamCursor &Stream = F.Stream;

  if (Stream.EnterSubBlock(AST_BLOCK_ID)) {
    Error("malformed block record in AST file");
    return true;
  }

  // Read all of the records and blocks for the AST file.
  RecordData Record;
  while (!Stream.AtEndOfStream()) {
    unsigned Code = Stream.ReadCode();
    if (Code == llvm::bitc::END_BLOCK) {
      if (Stream.ReadBlockEnd()) {
        Error("error at end of module block in AST file");
        return true;
      }

      DeclContext *DC = Context.getTranslationUnitDecl();
      if (!DC->hasExternalVisibleStorage() && DC->hasExternalLexicalStorage())
        DC->setMustBuildLookupTable();

      return false;
    }

    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      switch (Stream.ReadSubBlockID()) {
      case DECLTYPES_BLOCK_ID:
        // We lazily load the decls block, but we want to set up the
        // DeclsCursor cursor to point into it.  Clone our current bitcode
        // cursor to it, enter the block and read the abbrevs in that block.
        // With the main cursor, we just skip over it.
        F.DeclsCursor = Stream;
        if (Stream.SkipBlock() ||  // Skip with the main cursor.
            // Read the abbrevs.
            ReadBlockAbbrevs(F.DeclsCursor, DECLTYPES_BLOCK_ID)) {
          Error("malformed block record in AST file");
          return true;
        }
        break;

      case DECL_UPDATES_BLOCK_ID:
        if (Stream.SkipBlock()) {
          Error("malformed block record in AST file");
          return true;
        }
        break;

      case PREPROCESSOR_BLOCK_ID:
        F.MacroCursor = Stream;
        if (!PP.getExternalSource())
          PP.setExternalSource(this);

        if (Stream.SkipBlock() ||
            ReadBlockAbbrevs(F.MacroCursor, PREPROCESSOR_BLOCK_ID)) {
          Error("malformed block record in AST file");
          return true;
        }
        F.MacroStartOffset = F.MacroCursor.GetCurrentBitNo();
        break;

      case PREPROCESSOR_DETAIL_BLOCK_ID:
        F.PreprocessorDetailCursor = Stream;
        if (Stream.SkipBlock() ||
            ReadBlockAbbrevs(F.PreprocessorDetailCursor, 
                             PREPROCESSOR_DETAIL_BLOCK_ID)) {
          Error("malformed preprocessor detail record in AST file");
          return true;
        }
        F.PreprocessorDetailStartOffset
          = F.PreprocessorDetailCursor.GetCurrentBitNo();
          
        if (!PP.getPreprocessingRecord())
          PP.createPreprocessingRecord(/*RecordConditionalDirectives=*/false);
        if (!PP.getPreprocessingRecord()->getExternalSource())
          PP.getPreprocessingRecord()->SetExternalSource(*this);
        break;
        
      case SOURCE_MANAGER_BLOCK_ID:
        if (ReadSourceManagerBlock(F))
          return true;
        break;

      case SUBMODULE_BLOCK_ID:
        if (ReadSubmoduleBlock(F))
          return true;
        break;

      case COMMENTS_BLOCK_ID: {
        llvm::BitstreamCursor C = Stream;
        if (Stream.SkipBlock() ||
            ReadBlockAbbrevs(C, COMMENTS_BLOCK_ID)) {
          Error("malformed comments block in AST file");
          return true;
        }
        CommentsCursors.push_back(std::make_pair(C, &F));
        break;
      }

      default:
        if (!Stream.SkipBlock())
          break;
        Error("malformed block record in AST file");
        return true;
      }
      continue;
    }

    if (Code == llvm::bitc::DEFINE_ABBREV) {
      Stream.ReadAbbrevRecord();
      continue;
    }

    // Read and process a record.
    Record.clear();
    const char *BlobStart = 0;
    unsigned BlobLen = 0;
    switch ((ASTRecordTypes)Stream.ReadRecord(Code, Record,
                                              &BlobStart, &BlobLen)) {
    default:  // Default behavior: ignore.
      break;

    case TYPE_OFFSET: {
      if (F.LocalNumTypes != 0) {
        Error("duplicate TYPE_OFFSET record in AST file");
        return true;
      }
      F.TypeOffsets = (const uint32_t *)BlobStart;
      F.LocalNumTypes = Record[0];
      unsigned LocalBaseTypeIndex = Record[1];
      F.BaseTypeIndex = getTotalNumTypes();
        
      if (F.LocalNumTypes > 0) {
        // Introduce the global -> local mapping for types within this module.
        GlobalTypeMap.insert(std::make_pair(getTotalNumTypes(), &F));
        
        // Introduce the local -> global mapping for types within this module.
        F.TypeRemap.insertOrReplace(
          std::make_pair(LocalBaseTypeIndex, 
                         F.BaseTypeIndex - LocalBaseTypeIndex));
        
        TypesLoaded.resize(TypesLoaded.size() + F.LocalNumTypes);
      }
      break;
    }
        
    case DECL_OFFSET: {
      if (F.LocalNumDecls != 0) {
        Error("duplicate DECL_OFFSET record in AST file");
        return true;
      }
      F.DeclOffsets = (const DeclOffset *)BlobStart;
      F.LocalNumDecls = Record[0];
      unsigned LocalBaseDeclID = Record[1];
      F.BaseDeclID = getTotalNumDecls();
        
      if (F.LocalNumDecls > 0) {
        // Introduce the global -> local mapping for declarations within this 
        // module.
        GlobalDeclMap.insert(
          std::make_pair(getTotalNumDecls() + NUM_PREDEF_DECL_IDS, &F));
        
        // Introduce the local -> global mapping for declarations within this
        // module.
        F.DeclRemap.insertOrReplace(
          std::make_pair(LocalBaseDeclID, F.BaseDeclID - LocalBaseDeclID));
        
        // Introduce the global -> local mapping for declarations within this
        // module.
        F.GlobalToLocalDeclIDs[&F] = LocalBaseDeclID;
        
        DeclsLoaded.resize(DeclsLoaded.size() + F.LocalNumDecls);
      }
      break;
    }
        
    case TU_UPDATE_LEXICAL: {
      DeclContext *TU = Context.getTranslationUnitDecl();
      DeclContextInfo &Info = F.DeclContextInfos[TU];
      Info.LexicalDecls = reinterpret_cast<const KindDeclIDPair *>(BlobStart);
      Info.NumLexicalDecls 
        = static_cast<unsigned int>(BlobLen / sizeof(KindDeclIDPair));
      TU->setHasExternalLexicalStorage(true);
      break;
    }

    case UPDATE_VISIBLE: {
      unsigned Idx = 0;
      serialization::DeclID ID = ReadDeclID(F, Record, Idx);
      ASTDeclContextNameLookupTable *Table =
        ASTDeclContextNameLookupTable::Create(
                        (const unsigned char *)BlobStart + Record[Idx++],
                        (const unsigned char *)BlobStart,
                        ASTDeclContextNameLookupTrait(*this, F));
      if (ID == PREDEF_DECL_TRANSLATION_UNIT_ID) { // Is it the TU?
        DeclContext *TU = Context.getTranslationUnitDecl();
        F.DeclContextInfos[TU].NameLookupTableData = Table;
        TU->setHasExternalVisibleStorage(true);
      } else
        PendingVisibleUpdates[ID].push_back(std::make_pair(Table, &F));
      break;
    }

    case IDENTIFIER_TABLE:
      F.IdentifierTableData = BlobStart;
      if (Record[0]) {
        F.IdentifierLookupTable
          = ASTIdentifierLookupTable::Create(
                       (const unsigned char *)F.IdentifierTableData + Record[0],
                       (const unsigned char *)F.IdentifierTableData,
                       ASTIdentifierLookupTrait(*this, F));
        
        PP.getIdentifierTable().setExternalIdentifierLookup(this);
      }
      break;

    case IDENTIFIER_OFFSET: {
      if (F.LocalNumIdentifiers != 0) {
        Error("duplicate IDENTIFIER_OFFSET record in AST file");
        return true;
      }
      F.IdentifierOffsets = (const uint32_t *)BlobStart;
      F.LocalNumIdentifiers = Record[0];
      unsigned LocalBaseIdentifierID = Record[1];
      F.BaseIdentifierID = getTotalNumIdentifiers();
        
      if (F.LocalNumIdentifiers > 0) {
        // Introduce the global -> local mapping for identifiers within this
        // module.
        GlobalIdentifierMap.insert(std::make_pair(getTotalNumIdentifiers() + 1, 
                                                  &F));
        
        // Introduce the local -> global mapping for identifiers within this
        // module.
        F.IdentifierRemap.insertOrReplace(
          std::make_pair(LocalBaseIdentifierID,
                         F.BaseIdentifierID - LocalBaseIdentifierID));
        
        IdentifiersLoaded.resize(IdentifiersLoaded.size() 
                                 + F.LocalNumIdentifiers);
      }
      break;
    }

    case EXTERNAL_DEFINITIONS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        ExternalDefinitions.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case SPECIAL_TYPES:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        SpecialTypes.push_back(getGlobalTypeID(F, Record[I]));
      break;

    case STATISTICS:
      TotalNumStatements += Record[0];
      TotalNumMacros += Record[1];
      TotalLexicalDeclContexts += Record[2];
      TotalVisibleDeclContexts += Record[3];
      break;

    case UNUSED_FILESCOPED_DECLS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        UnusedFileScopedDecls.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case DELEGATING_CTORS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        DelegatingCtorDecls.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case WEAK_UNDECLARED_IDENTIFIERS:
      if (Record.size() % 4 != 0) {
        Error("invalid weak identifiers record");
        return true;
      }
        
      // FIXME: Ignore weak undeclared identifiers from non-original PCH 
      // files. This isn't the way to do it :)
      WeakUndeclaredIdentifiers.clear();
        
      // Translate the weak, undeclared identifiers into global IDs.
      for (unsigned I = 0, N = Record.size(); I < N; /* in loop */) {
        WeakUndeclaredIdentifiers.push_back(
          getGlobalIdentifierID(F, Record[I++]));
        WeakUndeclaredIdentifiers.push_back(
          getGlobalIdentifierID(F, Record[I++]));
        WeakUndeclaredIdentifiers.push_back(
          ReadSourceLocation(F, Record, I).getRawEncoding());
        WeakUndeclaredIdentifiers.push_back(Record[I++]);
      }
      break;

    case LOCALLY_SCOPED_EXTERNAL_DECLS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        LocallyScopedExternalDecls.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case SELECTOR_OFFSETS: {
      F.SelectorOffsets = (const uint32_t *)BlobStart;
      F.LocalNumSelectors = Record[0];
      unsigned LocalBaseSelectorID = Record[1];
      F.BaseSelectorID = getTotalNumSelectors();
        
      if (F.LocalNumSelectors > 0) {
        // Introduce the global -> local mapping for selectors within this 
        // module.
        GlobalSelectorMap.insert(std::make_pair(getTotalNumSelectors()+1, &F));
        
        // Introduce the local -> global mapping for selectors within this 
        // module.
        F.SelectorRemap.insertOrReplace(
          std::make_pair(LocalBaseSelectorID,
                         F.BaseSelectorID - LocalBaseSelectorID));

        SelectorsLoaded.resize(SelectorsLoaded.size() + F.LocalNumSelectors);        
      }
      break;
    }
        
    case METHOD_POOL:
      F.SelectorLookupTableData = (const unsigned char *)BlobStart;
      if (Record[0])
        F.SelectorLookupTable
          = ASTSelectorLookupTable::Create(
                        F.SelectorLookupTableData + Record[0],
                        F.SelectorLookupTableData,
                        ASTSelectorLookupTrait(*this, F));
      TotalNumMethodPoolEntries += Record[1];
      break;

    case REFERENCED_SELECTOR_POOL:
      if (!Record.empty()) {
        for (unsigned Idx = 0, N = Record.size() - 1; Idx < N; /* in loop */) {
          ReferencedSelectorsData.push_back(getGlobalSelectorID(F, 
                                                                Record[Idx++]));
          ReferencedSelectorsData.push_back(ReadSourceLocation(F, Record, Idx).
                                              getRawEncoding());
        }
      }
      break;

    case PP_COUNTER_VALUE:
      if (!Record.empty() && Listener)
        Listener->ReadCounter(F, Record[0]);
      break;
      
    case FILE_SORTED_DECLS:
      F.FileSortedDecls = (const DeclID *)BlobStart;
      F.NumFileSortedDecls = Record[0];
      break;

    case SOURCE_LOCATION_OFFSETS: {
      F.SLocEntryOffsets = (const uint32_t *)BlobStart;
      F.LocalNumSLocEntries = Record[0];
      unsigned SLocSpaceSize = Record[1];
      llvm::tie(F.SLocEntryBaseID, F.SLocEntryBaseOffset) =
          SourceMgr.AllocateLoadedSLocEntries(F.LocalNumSLocEntries,
                                              SLocSpaceSize);
      // Make our entry in the range map. BaseID is negative and growing, so
      // we invert it. Because we invert it, though, we need the other end of
      // the range.
      unsigned RangeStart =
          unsigned(-F.SLocEntryBaseID) - F.LocalNumSLocEntries + 1;
      GlobalSLocEntryMap.insert(std::make_pair(RangeStart, &F));
      F.FirstLoc = SourceLocation::getFromRawEncoding(F.SLocEntryBaseOffset);

      // SLocEntryBaseOffset is lower than MaxLoadedOffset and decreasing.
      assert((F.SLocEntryBaseOffset & (1U << 31U)) == 0);
      GlobalSLocOffsetMap.insert(
          std::make_pair(SourceManager::MaxLoadedOffset - F.SLocEntryBaseOffset
                           - SLocSpaceSize,&F));

      // Initialize the remapping table.
      // Invalid stays invalid.
      F.SLocRemap.insert(std::make_pair(0U, 0));
      // This module. Base was 2 when being compiled.
      F.SLocRemap.insert(std::make_pair(2U,
                                  static_cast<int>(F.SLocEntryBaseOffset - 2)));
      
      TotalNumSLocEntries += F.LocalNumSLocEntries;
      break;
    }

    case MODULE_OFFSET_MAP: {
      // Additional remapping information.
      const unsigned char *Data = (const unsigned char*)BlobStart;
      const unsigned char *DataEnd = Data + BlobLen;
      
      // Continuous range maps we may be updating in our module.
      ContinuousRangeMap<uint32_t, int, 2>::Builder SLocRemap(F.SLocRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder 
        IdentifierRemap(F.IdentifierRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder
        MacroRemap(F.MacroRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder
        PreprocessedEntityRemap(F.PreprocessedEntityRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder 
        SubmoduleRemap(F.SubmoduleRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder 
        SelectorRemap(F.SelectorRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder DeclRemap(F.DeclRemap);
      ContinuousRangeMap<uint32_t, int, 2>::Builder TypeRemap(F.TypeRemap);

      while(Data < DataEnd) {
        uint16_t Len = io::ReadUnalignedLE16(Data);
        StringRef Name = StringRef((const char*)Data, Len);
        Data += Len;
        ModuleFile *OM = ModuleMgr.lookup(Name);
        if (!OM) {
          Error("SourceLocation remap refers to unknown module");
          return true;
        }

        uint32_t SLocOffset = io::ReadUnalignedLE32(Data);
        uint32_t IdentifierIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t MacroIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t PreprocessedEntityIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t SubmoduleIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t SelectorIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t DeclIDOffset = io::ReadUnalignedLE32(Data);
        uint32_t TypeIndexOffset = io::ReadUnalignedLE32(Data);
        
        // Source location offset is mapped to OM->SLocEntryBaseOffset.
        SLocRemap.insert(std::make_pair(SLocOffset,
          static_cast<int>(OM->SLocEntryBaseOffset - SLocOffset)));
        IdentifierRemap.insert(
          std::make_pair(IdentifierIDOffset, 
                         OM->BaseIdentifierID - IdentifierIDOffset));
        MacroRemap.insert(std::make_pair(MacroIDOffset,
                                         OM->BaseMacroID - MacroIDOffset));
        PreprocessedEntityRemap.insert(
          std::make_pair(PreprocessedEntityIDOffset, 
            OM->BasePreprocessedEntityID - PreprocessedEntityIDOffset));
        SubmoduleRemap.insert(std::make_pair(SubmoduleIDOffset, 
                                      OM->BaseSubmoduleID - SubmoduleIDOffset));
        SelectorRemap.insert(std::make_pair(SelectorIDOffset, 
                               OM->BaseSelectorID - SelectorIDOffset));
        DeclRemap.insert(std::make_pair(DeclIDOffset, 
                                        OM->BaseDeclID - DeclIDOffset));
        
        TypeRemap.insert(std::make_pair(TypeIndexOffset, 
                                    OM->BaseTypeIndex - TypeIndexOffset));

        // Global -> local mappings.
        F.GlobalToLocalDeclIDs[OM] = DeclIDOffset;
      }
      break;
    }

    case SOURCE_MANAGER_LINE_TABLE:
      if (ParseLineTable(F, Record))
        return true;
      break;

    case SOURCE_LOCATION_PRELOADS: {
      // Need to transform from the local view (1-based IDs) to the global view,
      // which is based off F.SLocEntryBaseID.
      if (!F.PreloadSLocEntries.empty()) {
        Error("Multiple SOURCE_LOCATION_PRELOADS records in AST file");
        return true;
      }
      
      F.PreloadSLocEntries.swap(Record);
      break;
    }

    case EXT_VECTOR_DECLS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        ExtVectorDecls.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case VTABLE_USES:
      if (Record.size() % 3 != 0) {
        Error("Invalid VTABLE_USES record");
        return true;
      }
        
      // Later tables overwrite earlier ones.
      // FIXME: Modules will have some trouble with this. This is clearly not
      // the right way to do this.
      VTableUses.clear();
        
      for (unsigned Idx = 0, N = Record.size(); Idx != N; /* In loop */) {
        VTableUses.push_back(getGlobalDeclID(F, Record[Idx++]));
        VTableUses.push_back(
          ReadSourceLocation(F, Record, Idx).getRawEncoding());
        VTableUses.push_back(Record[Idx++]);
      }
      break;

    case DYNAMIC_CLASSES:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        DynamicClasses.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case PENDING_IMPLICIT_INSTANTIATIONS:
      if (PendingInstantiations.size() % 2 != 0) {
        Error("Invalid existing PendingInstantiations");
        return true;
      }

      if (Record.size() % 2 != 0) {
        Error("Invalid PENDING_IMPLICIT_INSTANTIATIONS block");
        return true;
      }

      for (unsigned I = 0, N = Record.size(); I != N; /* in loop */) {
        PendingInstantiations.push_back(getGlobalDeclID(F, Record[I++]));
        PendingInstantiations.push_back(
          ReadSourceLocation(F, Record, I).getRawEncoding());
      }
      break;

    case SEMA_DECL_REFS:
      // Later tables overwrite earlier ones.
      // FIXME: Modules will have some trouble with this.
      SemaDeclRefs.clear();
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        SemaDeclRefs.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case PPD_ENTITIES_OFFSETS: {
      F.PreprocessedEntityOffsets = (const PPEntityOffset *)BlobStart;
      assert(BlobLen % sizeof(PPEntityOffset) == 0);
      F.NumPreprocessedEntities = BlobLen / sizeof(PPEntityOffset);

      unsigned LocalBasePreprocessedEntityID = Record[0];
      
      unsigned StartingID;
      if (!PP.getPreprocessingRecord())
        PP.createPreprocessingRecord(/*RecordConditionalDirectives=*/false);
      if (!PP.getPreprocessingRecord()->getExternalSource())
        PP.getPreprocessingRecord()->SetExternalSource(*this);
      StartingID 
        = PP.getPreprocessingRecord()
            ->allocateLoadedEntities(F.NumPreprocessedEntities);
      F.BasePreprocessedEntityID = StartingID;

      if (F.NumPreprocessedEntities > 0) {
        // Introduce the global -> local mapping for preprocessed entities in
        // this module.
        GlobalPreprocessedEntityMap.insert(std::make_pair(StartingID, &F));
       
        // Introduce the local -> global mapping for preprocessed entities in
        // this module.
        F.PreprocessedEntityRemap.insertOrReplace(
          std::make_pair(LocalBasePreprocessedEntityID,
            F.BasePreprocessedEntityID - LocalBasePreprocessedEntityID));
      }

      break;
    }
        
    case DECL_UPDATE_OFFSETS: {
      if (Record.size() % 2 != 0) {
        Error("invalid DECL_UPDATE_OFFSETS block in AST file");
        return true;
      }
      for (unsigned I = 0, N = Record.size(); I != N; I += 2)
        DeclUpdateOffsets[getGlobalDeclID(F, Record[I])]
          .push_back(std::make_pair(&F, Record[I+1]));
      break;
    }

    case DECL_REPLACEMENTS: {
      if (Record.size() % 3 != 0) {
        Error("invalid DECL_REPLACEMENTS block in AST file");
        return true;
      }
      for (unsigned I = 0, N = Record.size(); I != N; I += 3)
        ReplacedDecls[getGlobalDeclID(F, Record[I])]
          = ReplacedDeclInfo(&F, Record[I+1], Record[I+2]);
      break;
    }

    case OBJC_CATEGORIES_MAP: {
      if (F.LocalNumObjCCategoriesInMap != 0) {
        Error("duplicate OBJC_CATEGORIES_MAP record in AST file");
        return true;
      }
      
      F.LocalNumObjCCategoriesInMap = Record[0];
      F.ObjCCategoriesMap = (const ObjCCategoriesInfo *)BlobStart;
      break;
    }
        
    case OBJC_CATEGORIES:
      F.ObjCCategories.swap(Record);
      break;
        
    case CXX_BASE_SPECIFIER_OFFSETS: {
      if (F.LocalNumCXXBaseSpecifiers != 0) {
        Error("duplicate CXX_BASE_SPECIFIER_OFFSETS record in AST file");
        return true;
      }
      
      F.LocalNumCXXBaseSpecifiers = Record[0];
      F.CXXBaseSpecifiersOffsets = (const uint32_t *)BlobStart;
      NumCXXBaseSpecifiersLoaded += F.LocalNumCXXBaseSpecifiers;
      break;
    }

    case DIAG_PRAGMA_MAPPINGS:
      if (F.PragmaDiagMappings.empty())
        F.PragmaDiagMappings.swap(Record);
      else
        F.PragmaDiagMappings.insert(F.PragmaDiagMappings.end(),
                                    Record.begin(), Record.end());
      break;
        
    case CUDA_SPECIAL_DECL_REFS:
      // Later tables overwrite earlier ones.
      // FIXME: Modules will have trouble with this.
      CUDASpecialDeclRefs.clear();
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        CUDASpecialDeclRefs.push_back(getGlobalDeclID(F, Record[I]));
      break;

    case HEADER_SEARCH_TABLE: {
      F.HeaderFileInfoTableData = BlobStart;
      F.LocalNumHeaderFileInfos = Record[1];
      F.HeaderFileFrameworkStrings = BlobStart + Record[2];
      if (Record[0]) {
        F.HeaderFileInfoTable
          = HeaderFileInfoLookupTable::Create(
                   (const unsigned char *)F.HeaderFileInfoTableData + Record[0],
                   (const unsigned char *)F.HeaderFileInfoTableData,
                   HeaderFileInfoTrait(*this, F, 
                                       &PP.getHeaderSearchInfo(),
                                       BlobStart + Record[2]));
        
        PP.getHeaderSearchInfo().SetExternalSource(this);
        if (!PP.getHeaderSearchInfo().getExternalLookup())
          PP.getHeaderSearchInfo().SetExternalLookup(this);
      }
      break;
    }
        
    case FP_PRAGMA_OPTIONS:
      // Later tables overwrite earlier ones.
      FPPragmaOptions.swap(Record);
      break;

    case OPENCL_EXTENSIONS:
      // Later tables overwrite earlier ones.
      OpenCLExtensions.swap(Record);
      break;

    case TENTATIVE_DEFINITIONS:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        TentativeDefinitions.push_back(getGlobalDeclID(F, Record[I]));
      break;
        
    case KNOWN_NAMESPACES:
      for (unsigned I = 0, N = Record.size(); I != N; ++I)
        KnownNamespaces.push_back(getGlobalDeclID(F, Record[I]));
      break;
        
    case IMPORTED_MODULES: {
      if (F.Kind != MK_Module) {
        // If we aren't loading a module (which has its own exports), make
        // all of the imported modules visible.
        // FIXME: Deal with macros-only imports.
        for (unsigned I = 0, N = Record.size(); I != N; ++I) {
          if (unsigned GlobalID = getGlobalSubmoduleID(F, Record[I]))
            ImportedModules.push_back(GlobalID);
        }
      }
      break;
    }

    case LOCAL_REDECLARATIONS: {
      F.RedeclarationChains.swap(Record);
      break;
    }
        
    case LOCAL_REDECLARATIONS_MAP: {
      if (F.LocalNumRedeclarationsInMap != 0) {
        Error("duplicate LOCAL_REDECLARATIONS_MAP record in AST file");
        return true;
      }
      
      F.LocalNumRedeclarationsInMap = Record[0];
      F.RedeclarationsMap = (const LocalRedeclarationsInfo *)BlobStart;
      break;
    }
        
    case MERGED_DECLARATIONS: {
      for (unsigned Idx = 0; Idx < Record.size(); /* increment in loop */) {
        GlobalDeclID CanonID = getGlobalDeclID(F, Record[Idx++]);
        SmallVectorImpl<GlobalDeclID> &Decls = StoredMergedDecls[CanonID];
        for (unsigned N = Record[Idx++]; N > 0; --N)
          Decls.push_back(getGlobalDeclID(F, Record[Idx++]));
      }
      break;
    }

    case MACRO_OFFSET: {
      if (F.LocalNumMacros != 0) {
        Error("duplicate MACRO_OFFSET record in AST file");
        return true;
      }
      F.MacroOffsets = (const uint32_t *)BlobStart;
      F.LocalNumMacros = Record[0];
      unsigned LocalBaseMacroID = Record[1];
      F.BaseMacroID = getTotalNumMacros();

      if (F.LocalNumMacros > 0) {
        // Introduce the global -> local mapping for macros within this module.
        GlobalMacroMap.insert(std::make_pair(getTotalNumMacros() + 1, &F));

        // Introduce the local -> global mapping for macros within this module.
        F.MacroRemap.insertOrReplace(
          std::make_pair(LocalBaseMacroID,
                         F.BaseMacroID - LocalBaseMacroID));

        MacrosLoaded.resize(MacrosLoaded.size() + F.LocalNumMacros);
      }
      break;
    }

    case MACRO_UPDATES: {
      for (unsigned I = 0, N = Record.size(); I != N; /* in loop */) {
        MacroID ID = getGlobalMacroID(F, Record[I++]);
        if (I == N)
          break;

        SourceLocation UndefLoc = ReadSourceLocation(F, Record, I);
        SubmoduleID SubmoduleID = getGlobalSubmoduleID(F, Record[I++]);;
        MacroUpdate Update;
        Update.UndefLoc = UndefLoc;
        MacroUpdates[ID].push_back(std::make_pair(SubmoduleID, Update));
      }
      break;
    }
    }
  }
  Error("premature end of bitstream in AST file");
  return true;
}

void ASTReader::makeNamesVisible(const HiddenNames &Names) {
  for (unsigned I = 0, N = Names.size(); I != N; ++I) {
    switch (Names[I].getKind()) {
    case HiddenName::Declaration:
      Names[I].getDecl()->Hidden = false;
      break;

    case HiddenName::MacroVisibility: {
      std::pair<IdentifierInfo *, MacroInfo *> Macro = Names[I].getMacro();
      Macro.second->setHidden(!Macro.second->isPublic());
      if (Macro.second->isDefined()) {
        PP.makeLoadedMacroInfoVisible(Macro.first, Macro.second);
      }
      break;
    }

    case HiddenName::MacroUndef: {
      std::pair<IdentifierInfo *, MacroInfo *> Macro = Names[I].getMacro();
      if (Macro.second->isDefined()) {
        Macro.second->setUndefLoc(Names[I].getMacroUndefLoc());
        if (PPMutationListener *Listener = PP.getPPMutationListener())
          Listener->UndefinedMacro(Macro.second);
        PP.makeLoadedMacroInfoVisible(Macro.first, Macro.second);
      }
      break;
    }
    }
  }
}

void ASTReader::makeModuleVisible(Module *Mod, 
                                  Module::NameVisibilityKind NameVisibility) {
  llvm::SmallPtrSet<Module *, 4> Visited;
  llvm::SmallVector<Module *, 4> Stack;
  Stack.push_back(Mod);  
  while (!Stack.empty()) {
    Mod = Stack.back();
    Stack.pop_back();

    if (NameVisibility <= Mod->NameVisibility) {
      // This module already has this level of visibility (or greater), so 
      // there is nothing more to do.
      continue;
    }
    
    if (!Mod->isAvailable()) {
      // Modules that aren't available cannot be made visible.
      continue;
    }

    // Update the module's name visibility.
    Mod->NameVisibility = NameVisibility;
    
    // If we've already deserialized any names from this module,
    // mark them as visible.
    HiddenNamesMapType::iterator Hidden = HiddenNamesMap.find(Mod);
    if (Hidden != HiddenNamesMap.end()) {
      makeNamesVisible(Hidden->second);
      HiddenNamesMap.erase(Hidden);
    }
    
    // Push any non-explicit submodules onto the stack to be marked as
    // visible.
    for (Module::submodule_iterator Sub = Mod->submodule_begin(),
                                 SubEnd = Mod->submodule_end();
         Sub != SubEnd; ++Sub) {
      if (!(*Sub)->IsExplicit && Visited.insert(*Sub))
        Stack.push_back(*Sub);
    }
    
    // Push any exported modules onto the stack to be marked as visible.
    bool AnyWildcard = false;
    bool UnrestrictedWildcard = false;
    llvm::SmallVector<Module *, 4> WildcardRestrictions;
    for (unsigned I = 0, N = Mod->Exports.size(); I != N; ++I) {
      Module *Exported = Mod->Exports[I].getPointer();
      if (!Mod->Exports[I].getInt()) {
        // Export a named module directly; no wildcards involved.
        if (Visited.insert(Exported))
          Stack.push_back(Exported);
        
        continue;
      }
      
      // Wildcard export: export all of the imported modules that match
      // the given pattern.
      AnyWildcard = true;
      if (UnrestrictedWildcard)
        continue;

      if (Module *Restriction = Mod->Exports[I].getPointer())
        WildcardRestrictions.push_back(Restriction);
      else {
        WildcardRestrictions.clear();
        UnrestrictedWildcard = true;
      }
    }
    
    // If there were any wildcards, push any imported modules that were
    // re-exported by the wildcard restriction.
    if (!AnyWildcard)
      continue;
    
    for (unsigned I = 0, N = Mod->Imports.size(); I != N; ++I) {
      Module *Imported = Mod->Imports[I];
      if (!Visited.insert(Imported))
        continue;
      
      bool Acceptable = UnrestrictedWildcard;
      if (!Acceptable) {
        // Check whether this module meets one of the restrictions.
        for (unsigned R = 0, NR = WildcardRestrictions.size(); R != NR; ++R) {
          Module *Restriction = WildcardRestrictions[R];
          if (Imported == Restriction || Imported->isSubModuleOf(Restriction)) {
            Acceptable = true;
            break;
          }
        }
      }
      
      if (!Acceptable)
        continue;
      
      Stack.push_back(Imported);
    }
  }
}

ASTReader::ASTReadResult ASTReader::ReadAST(const std::string &FileName,
                                            ModuleKind Type,
                                            unsigned ClientLoadCapabilities) {
  // Bump the generation number.
  unsigned PreviousGeneration = CurrentGeneration++;

  // Load the core of the AST files.
  llvm::SmallVector<ModuleFile *, 4> Loaded;
  switch(ReadASTCore(FileName, Type, /*ImportedBy=*/0, Loaded,
                     ClientLoadCapabilities)) {
  case Failure: return Failure;
  case OutOfDate: return OutOfDate;
  case VersionMismatch: return VersionMismatch;
  case ConfigurationMismatch: return ConfigurationMismatch;
  case HadErrors: return HadErrors;
  case Success: break;
  }

  // Here comes stuff that we only do once the entire chain is loaded.

  // Load the AST blocks of all of the modules that we loaded.
  for (llvm::SmallVectorImpl<ModuleFile *>::iterator M = Loaded.begin(),
                                                  MEnd = Loaded.end();
       M != MEnd; ++M) {
    ModuleFile &F = **M;

    // Read the AST block.
    if (ReadASTBlock(F))
      return Failure;

    // Once read, set the ModuleFile bit base offset and update the size in 
    // bits of all files we've seen.
    F.GlobalBitOffset = TotalModulesSizeInBits;
    TotalModulesSizeInBits += F.SizeInBits;
    GlobalBitOffsetsMap.insert(std::make_pair(F.GlobalBitOffset, &F));
    
    // Preload SLocEntries.
    for (unsigned I = 0, N = F.PreloadSLocEntries.size(); I != N; ++I) {
      int Index = int(F.PreloadSLocEntries[I] - 1) + F.SLocEntryBaseID;
      // Load it through the SourceManager and don't call ReadSLocEntry()
      // directly because the entry may have already been loaded in which case
      // calling ReadSLocEntry() directly would trigger an assertion in
      // SourceManager.
      SourceMgr.getLoadedSLocEntryByID(Index);
    }
  }

  // Mark all of the identifiers in the identifier table as being out of date,
  // so that various accessors know to check the loaded modules when the
  // identifier is used.
  for (IdentifierTable::iterator Id = PP.getIdentifierTable().begin(),
                              IdEnd = PP.getIdentifierTable().end();
       Id != IdEnd; ++Id)
    Id->second->setOutOfDate(true);
  
  // Resolve any unresolved module exports.
  for (unsigned I = 0, N = UnresolvedModuleImportExports.size(); I != N; ++I) {
    UnresolvedModuleImportExport &Unresolved = UnresolvedModuleImportExports[I];
    SubmoduleID GlobalID = getGlobalSubmoduleID(*Unresolved.File,Unresolved.ID);
    Module *ResolvedMod = getSubmodule(GlobalID);
    
    if (Unresolved.IsImport) {
      if (ResolvedMod)
        Unresolved.Mod->Imports.push_back(ResolvedMod);
      continue;
    }

    if (ResolvedMod || Unresolved.IsWildcard)
      Unresolved.Mod->Exports.push_back(
        Module::ExportDecl(ResolvedMod, Unresolved.IsWildcard));
  }
  UnresolvedModuleImportExports.clear();
  
  InitializeContext();

  if (DeserializationListener)
    DeserializationListener->ReaderInitialized(this);

  ModuleFile &PrimaryModule = ModuleMgr.getPrimaryModule();
  if (!PrimaryModule.OriginalSourceFileID.isInvalid()) {
    PrimaryModule.OriginalSourceFileID 
      = FileID::get(PrimaryModule.SLocEntryBaseID
                    + PrimaryModule.OriginalSourceFileID.getOpaqueValue() - 1);

    // If this AST file is a precompiled preamble, then set the
    // preamble file ID of the source manager to the file source file
    // from which the preamble was built.
    if (Type == MK_Preamble) {
      SourceMgr.setPreambleFileID(PrimaryModule.OriginalSourceFileID);
    } else if (Type == MK_MainFile) {
      SourceMgr.setMainFileID(PrimaryModule.OriginalSourceFileID);
    }
  }
  
  // For any Objective-C class definitions we have already loaded, make sure
  // that we load any additional categories.
  for (unsigned I = 0, N = ObjCClassesLoaded.size(); I != N; ++I) {
    loadObjCCategories(ObjCClassesLoaded[I]->getGlobalID(), 
                       ObjCClassesLoaded[I],
                       PreviousGeneration);
  }
  
  return Success;
}

ASTReader::ASTReadResult
ASTReader::ReadASTCore(StringRef FileName,
                       ModuleKind Type,
                       ModuleFile *ImportedBy,
                       llvm::SmallVectorImpl<ModuleFile *> &Loaded,
                       unsigned ClientLoadCapabilities) {
  ModuleFile *M;
  bool NewModule;
  std::string ErrorStr;
  llvm::tie(M, NewModule) = ModuleMgr.addModule(FileName, Type, ImportedBy,
                                                CurrentGeneration, ErrorStr);

  if (!M) {
    // We couldn't load the module.
    std::string Msg = "Unable to load module \"" + FileName.str() + "\": "
      + ErrorStr;
    Error(Msg);
    return Failure;
  }

  if (!NewModule) {
    // We've already loaded this module.
    return Success;
  }

  // FIXME: This seems rather a hack. Should CurrentDir be part of the
  // module?
  if (FileName != "-") {
    CurrentDir = llvm::sys::path::parent_path(FileName);
    if (CurrentDir.empty()) CurrentDir = ".";
  }

  ModuleFile &F = *M;
  llvm::BitstreamCursor &Stream = F.Stream;
  Stream.init(F.StreamFile);
  F.SizeInBits = F.Buffer->getBufferSize() * 8;
  
  // Sniff for the signature.
  if (Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'P' ||
      Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'H') {
    Diag(diag::err_not_a_pch_file) << FileName;
    return Failure;
  }

  while (!Stream.AtEndOfStream()) {
    unsigned Code = Stream.ReadCode();

    if (Code != llvm::bitc::ENTER_SUBBLOCK) {
      Error("invalid record at top-level of AST file");
      return Failure;
    }

    unsigned BlockID = Stream.ReadSubBlockID();

    // We only know the control subblock ID.
    switch (BlockID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID:
      if (Stream.ReadBlockInfoBlock()) {
        Error("malformed BlockInfoBlock in AST file");
        return Failure;
      }
      break;
    case CONTROL_BLOCK_ID:
      switch (ReadControlBlock(F, Loaded, ClientLoadCapabilities)) {
      case Success:
        break;

      case Failure: return Failure;
      case OutOfDate: return OutOfDate;
      case VersionMismatch: return VersionMismatch;
      case ConfigurationMismatch: return ConfigurationMismatch;
      case HadErrors: return HadErrors;
      }
      break;
    case AST_BLOCK_ID:
      // Record that we've loaded this module.
      Loaded.push_back(M);
      return Success;

    default:
      if (Stream.SkipBlock()) {
        Error("malformed block record in AST file");
        return Failure;
      }
      break;
    }
  }
  
  return Success;
}

void ASTReader::InitializeContext() {  
  // If there's a listener, notify them that we "read" the translation unit.
  if (DeserializationListener)
    DeserializationListener->DeclRead(PREDEF_DECL_TRANSLATION_UNIT_ID, 
                                      Context.getTranslationUnitDecl());

  // Make sure we load the declaration update records for the translation unit,
  // if there are any.
  loadDeclUpdateRecords(PREDEF_DECL_TRANSLATION_UNIT_ID, 
                        Context.getTranslationUnitDecl());
  
  // FIXME: Find a better way to deal with collisions between these
  // built-in types. Right now, we just ignore the problem.
  
  // Load the special types.
  if (SpecialTypes.size() >= NumSpecialTypeIDs) {
    if (unsigned String = SpecialTypes[SPECIAL_TYPE_CF_CONSTANT_STRING]) {
      if (!Context.CFConstantStringTypeDecl)
        Context.setCFConstantStringType(GetType(String));
    }
    
    if (unsigned File = SpecialTypes[SPECIAL_TYPE_FILE]) {
      QualType FileType = GetType(File);
      if (FileType.isNull()) {
        Error("FILE type is NULL");
        return;
      }
      
      if (!Context.FILEDecl) {
        if (const TypedefType *Typedef = FileType->getAs<TypedefType>())
          Context.setFILEDecl(Typedef->getDecl());
        else {
          const TagType *Tag = FileType->getAs<TagType>();
          if (!Tag) {
            Error("Invalid FILE type in AST file");
            return;
          }
          Context.setFILEDecl(Tag->getDecl());
        }
      }
    }
    
    if (unsigned Jmp_buf = SpecialTypes[SPECIAL_TYPE_JMP_BUF]) {
      QualType Jmp_bufType = GetType(Jmp_buf);
      if (Jmp_bufType.isNull()) {
        Error("jmp_buf type is NULL");
        return;
      }
      
      if (!Context.jmp_bufDecl) {
        if (const TypedefType *Typedef = Jmp_bufType->getAs<TypedefType>())
          Context.setjmp_bufDecl(Typedef->getDecl());
        else {
          const TagType *Tag = Jmp_bufType->getAs<TagType>();
          if (!Tag) {
            Error("Invalid jmp_buf type in AST file");
            return;
          }
          Context.setjmp_bufDecl(Tag->getDecl());
        }
      }
    }
    
    if (unsigned Sigjmp_buf = SpecialTypes[SPECIAL_TYPE_SIGJMP_BUF]) {
      QualType Sigjmp_bufType = GetType(Sigjmp_buf);
      if (Sigjmp_bufType.isNull()) {
        Error("sigjmp_buf type is NULL");
        return;
      }
      
      if (!Context.sigjmp_bufDecl) {
        if (const TypedefType *Typedef = Sigjmp_bufType->getAs<TypedefType>())
          Context.setsigjmp_bufDecl(Typedef->getDecl());
        else {
          const TagType *Tag = Sigjmp_bufType->getAs<TagType>();
          assert(Tag && "Invalid sigjmp_buf type in AST file");
          Context.setsigjmp_bufDecl(Tag->getDecl());
        }
      }
    }

    if (unsigned ObjCIdRedef
          = SpecialTypes[SPECIAL_TYPE_OBJC_ID_REDEFINITION]) {
      if (Context.ObjCIdRedefinitionType.isNull())
        Context.ObjCIdRedefinitionType = GetType(ObjCIdRedef);
    }

    if (unsigned ObjCClassRedef
          = SpecialTypes[SPECIAL_TYPE_OBJC_CLASS_REDEFINITION]) {
      if (Context.ObjCClassRedefinitionType.isNull())
        Context.ObjCClassRedefinitionType = GetType(ObjCClassRedef);
    }

    if (unsigned ObjCSelRedef
          = SpecialTypes[SPECIAL_TYPE_OBJC_SEL_REDEFINITION]) {
      if (Context.ObjCSelRedefinitionType.isNull())
        Context.ObjCSelRedefinitionType = GetType(ObjCSelRedef);
    }

    if (unsigned Ucontext_t = SpecialTypes[SPECIAL_TYPE_UCONTEXT_T]) {
      QualType Ucontext_tType = GetType(Ucontext_t);
      if (Ucontext_tType.isNull()) {
        Error("ucontext_t type is NULL");
        return;
      }

      if (!Context.ucontext_tDecl) {
        if (const TypedefType *Typedef = Ucontext_tType->getAs<TypedefType>())
          Context.setucontext_tDecl(Typedef->getDecl());
        else {
          const TagType *Tag = Ucontext_tType->getAs<TagType>();
          assert(Tag && "Invalid ucontext_t type in AST file");
          Context.setucontext_tDecl(Tag->getDecl());
        }
      }
    }
  }
  
  ReadPragmaDiagnosticMappings(Context.getDiagnostics());

  // If there were any CUDA special declarations, deserialize them.
  if (!CUDASpecialDeclRefs.empty()) {
    assert(CUDASpecialDeclRefs.size() == 1 && "More decl refs than expected!");
    Context.setcudaConfigureCallDecl(
                           cast<FunctionDecl>(GetDecl(CUDASpecialDeclRefs[0])));
  }
  
  // Re-export any modules that were imported by a non-module AST file.
  for (unsigned I = 0, N = ImportedModules.size(); I != N; ++I) {
    if (Module *Imported = getSubmodule(ImportedModules[I]))
      makeModuleVisible(Imported, Module::AllVisible);
  }
  ImportedModules.clear();
}

void ASTReader::finalizeForWriting() {
  for (HiddenNamesMapType::iterator Hidden = HiddenNamesMap.begin(),
                                 HiddenEnd = HiddenNamesMap.end();
       Hidden != HiddenEnd; ++Hidden) {
    makeNamesVisible(Hidden->second);
  }
  HiddenNamesMap.clear();
}

/// \brief Retrieve the name of the original source file name
/// directly from the AST file, without actually loading the AST
/// file.
std::string ASTReader::getOriginalSourceFile(const std::string &ASTFileName,
                                             FileManager &FileMgr,
                                             DiagnosticsEngine &Diags) {
  // Open the AST file.
  std::string ErrStr;
  OwningPtr<llvm::MemoryBuffer> Buffer;
  Buffer.reset(FileMgr.getBufferForFile(ASTFileName, &ErrStr));
  if (!Buffer) {
    Diags.Report(diag::err_fe_unable_to_read_pch_file) << ASTFileName << ErrStr;
    return std::string();
  }

  // Initialize the stream
  llvm::BitstreamReader StreamFile;
  llvm::BitstreamCursor Stream;
  StreamFile.init((const unsigned char *)Buffer->getBufferStart(),
                  (const unsigned char *)Buffer->getBufferEnd());
  Stream.init(StreamFile);

  // Sniff for the signature.
  if (Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'P' ||
      Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'H') {
    Diags.Report(diag::err_fe_not_a_pch_file) << ASTFileName;
    return std::string();
  }

  RecordData Record;
  while (!Stream.AtEndOfStream()) {
    unsigned Code = Stream.ReadCode();

    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      unsigned BlockID = Stream.ReadSubBlockID();

      // We only know the AST subblock ID.
      switch (BlockID) {
      case CONTROL_BLOCK_ID:
        if (Stream.EnterSubBlock(CONTROL_BLOCK_ID)) {
          Diags.Report(diag::err_fe_pch_malformed_block) << ASTFileName;
          return std::string();
        }
        break;

      default:
        if (Stream.SkipBlock()) {
          Diags.Report(diag::err_fe_pch_malformed_block) << ASTFileName;
          return std::string();
        }
        break;
      }
      continue;
    }

    if (Code == llvm::bitc::END_BLOCK) {
      if (Stream.ReadBlockEnd()) {
        Diags.Report(diag::err_fe_pch_error_at_end_block) << ASTFileName;
        return std::string();
      }
      continue;
    }

    if (Code == llvm::bitc::DEFINE_ABBREV) {
      Stream.ReadAbbrevRecord();
      continue;
    }

    Record.clear();
    const char *BlobStart = 0;
    unsigned BlobLen = 0;
    if (Stream.ReadRecord(Code, Record, &BlobStart, &BlobLen) == ORIGINAL_FILE)
      return std::string(BlobStart, BlobLen);
  }

  return std::string();
}

namespace {
  class SimplePCHValidator : public ASTReaderListener {
    const LangOptions &ExistingLangOpts;
    const TargetOptions &ExistingTargetOpts;
    const PreprocessorOptions &ExistingPPOpts;
    FileManager &FileMgr;
    
  public:
    SimplePCHValidator(const LangOptions &ExistingLangOpts,
                       const TargetOptions &ExistingTargetOpts,
                       const PreprocessorOptions &ExistingPPOpts,
                       FileManager &FileMgr)
      : ExistingLangOpts(ExistingLangOpts),
        ExistingTargetOpts(ExistingTargetOpts),
        ExistingPPOpts(ExistingPPOpts),
        FileMgr(FileMgr)
    {
    }

    virtual bool ReadLanguageOptions(const LangOptions &LangOpts,
                                     bool Complain) {
      return checkLanguageOptions(ExistingLangOpts, LangOpts, 0);
    }
    virtual bool ReadTargetOptions(const TargetOptions &TargetOpts,
                                   bool Complain) {
      return checkTargetOptions(ExistingTargetOpts, TargetOpts, 0);
    }
    virtual bool ReadPreprocessorOptions(const PreprocessorOptions &PPOpts,
                                         bool Complain,
                                         std::string &SuggestedPredefines) {
      return checkPreprocessorOptions(ExistingPPOpts, PPOpts, 0, FileMgr,
                                      SuggestedPredefines);
    }
  };
}

bool ASTReader::readASTFileControlBlock(StringRef Filename,
                                        FileManager &FileMgr,
                                        ASTReaderListener &Listener) {
  // Open the AST file.
  std::string ErrStr;
  OwningPtr<llvm::MemoryBuffer> Buffer;
  Buffer.reset(FileMgr.getBufferForFile(Filename, &ErrStr));
  if (!Buffer) {
    return true;
  }

  // Initialize the stream
  llvm::BitstreamReader StreamFile;
  llvm::BitstreamCursor Stream;
  StreamFile.init((const unsigned char *)Buffer->getBufferStart(),
                  (const unsigned char *)Buffer->getBufferEnd());
  Stream.init(StreamFile);

  // Sniff for the signature.
  if (Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'P' ||
      Stream.Read(8) != 'C' ||
      Stream.Read(8) != 'H') {
    return true;
  }

  RecordData Record;
  bool InControlBlock = false;
  while (!Stream.AtEndOfStream()) {
    unsigned Code = Stream.ReadCode();

    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      unsigned BlockID = Stream.ReadSubBlockID();

      // We only know the control subblock ID.
      switch (BlockID) {
      case CONTROL_BLOCK_ID:
        if (Stream.EnterSubBlock(CONTROL_BLOCK_ID)) {
          return true;
        } else {
          InControlBlock = true;
        }
        break;

      default:
        if (Stream.SkipBlock())
          return true;
        break;
      }
      continue;
    }

    if (Code == llvm::bitc::END_BLOCK) {
      if (Stream.ReadBlockEnd()) {
        return true;
      }

      InControlBlock = false;
      continue;
    }

    if (Code == llvm::bitc::DEFINE_ABBREV) {
      Stream.ReadAbbrevRecord();
      continue;
    }

    Record.clear();
    const char *BlobStart = 0;
    unsigned BlobLen = 0;
    unsigned RecCode = Stream.ReadRecord(Code, Record, &BlobStart, &BlobLen);
    if (InControlBlock) {
      switch ((ControlRecordTypes)RecCode) {
      case METADATA: {
        if (Record[0] != VERSION_MAJOR) {
          return true;
        }

        const std::string &CurBranch = getClangFullRepositoryVersion();
        StringRef ASTBranch(BlobStart, BlobLen);
        if (StringRef(CurBranch) != ASTBranch)
          return true;

        break;
      }
      case LANGUAGE_OPTIONS:
        if (ParseLanguageOptions(Record, false, Listener))
          return true;
        break;

      case TARGET_OPTIONS:
        if (ParseTargetOptions(Record, false, Listener))
          return true;
        break;

      case DIAGNOSTIC_OPTIONS:
        if (ParseDiagnosticOptions(Record, false, Listener))
          return true;
        break;

      case FILE_SYSTEM_OPTIONS:
        if (ParseFileSystemOptions(Record, false, Listener))
          return true;
        break;

      case HEADER_SEARCH_OPTIONS:
        if (ParseHeaderSearchOptions(Record, false, Listener))
          return true;
        break;

      case PREPROCESSOR_OPTIONS: {
        std::string IgnoredSuggestedPredefines;
        if (ParsePreprocessorOptions(Record, false, Listener,
                                     IgnoredSuggestedPredefines))
          return true;
        break;
      }

      default:
        // No other validation to perform.
        break;
      }
    }
  }
  
  return false;
}


bool ASTReader::isAcceptableASTFile(StringRef Filename,
                                    FileManager &FileMgr,
                                    const LangOptions &LangOpts,
                                    const TargetOptions &TargetOpts,
                                    const PreprocessorOptions &PPOpts) {
  SimplePCHValidator validator(LangOpts, TargetOpts, PPOpts, FileMgr);
  return !readASTFileControlBlock(Filename, FileMgr, validator);
}

bool ASTReader::ReadSubmoduleBlock(ModuleFile &F) {
  // Enter the submodule block.
  if (F.Stream.EnterSubBlock(SUBMODULE_BLOCK_ID)) {
    Error("malformed submodule block record in AST file");
    return true;
  }

  ModuleMap &ModMap = PP.getHeaderSearchInfo().getModuleMap();
  bool First = true;
  Module *CurrentModule = 0;
  RecordData Record;
  while (true) {
    unsigned Code = F.Stream.ReadCode();
    if (Code == llvm::bitc::END_BLOCK) {
      if (F.Stream.ReadBlockEnd()) {
        Error("error at end of submodule block in AST file");
        return true;
      }
      return false;
    }
    
    if (Code == llvm::bitc::ENTER_SUBBLOCK) {
      // No known subblocks, always skip them.
      F.Stream.ReadSubBlockID();
      if (F.Stream.SkipBlock()) {
        Error("malformed block record in AST file");
        return true;
      }
      continue;
    }
    
    if (Code == llvm::bitc::DEFINE_ABBREV) {
      F.Stream.ReadAbbrevRecord();
      continue;
    }
    
    // Read a record.
    const char *BlobStart;
    unsigned BlobLen;
    Record.clear();
    switch (F.Stream.ReadRecord(Code, Record, &BlobStart, &BlobLen)) {
    default:  // Default behavior: ignore.
      break;
      
    case SUBMODULE_DEFINITION: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (Record.size() < 7) {
        Error("malformed module definition");
        return true;
      }
      
      StringRef Name(BlobStart, BlobLen);
      SubmoduleID GlobalID = getGlobalSubmoduleID(F, Record[0]);
      SubmoduleID Parent = getGlobalSubmoduleID(F, Record[1]);
      bool IsFramework = Record[2];
      bool IsExplicit = Record[3];
      bool IsSystem = Record[4];
      bool InferSubmodules = Record[5];
      bool InferExplicitSubmodules = Record[6];
      bool InferExportWildcard = Record[7];
      
      Module *ParentModule = 0;
      if (Parent)
        ParentModule = getSubmodule(Parent);
      
      // Retrieve this (sub)module from the module map, creating it if
      // necessary.
      CurrentModule = ModMap.findOrCreateModule(Name, ParentModule, 
                                                IsFramework, 
                                                IsExplicit).first;
      SubmoduleID GlobalIndex = GlobalID - NUM_PREDEF_SUBMODULE_IDS;
      if (GlobalIndex >= SubmodulesLoaded.size() ||
          SubmodulesLoaded[GlobalIndex]) {
        Error("too many submodules");
        return true;
      }
      
      CurrentModule->setASTFile(F.File);
      CurrentModule->IsFromModuleFile = true;
      CurrentModule->IsSystem = IsSystem || CurrentModule->IsSystem;
      CurrentModule->InferSubmodules = InferSubmodules;
      CurrentModule->InferExplicitSubmodules = InferExplicitSubmodules;
      CurrentModule->InferExportWildcard = InferExportWildcard;
      if (DeserializationListener)
        DeserializationListener->ModuleRead(GlobalID, CurrentModule);
      
      SubmodulesLoaded[GlobalIndex] = CurrentModule;
      break;
    }
        
    case SUBMODULE_UMBRELLA_HEADER: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (!CurrentModule)
        break;
      
      StringRef FileName(BlobStart, BlobLen);
      if (const FileEntry *Umbrella = PP.getFileManager().getFile(FileName)) {
        if (!CurrentModule->getUmbrellaHeader())
          ModMap.setUmbrellaHeader(CurrentModule, Umbrella);
        else if (CurrentModule->getUmbrellaHeader() != Umbrella) {
          Error("mismatched umbrella headers in submodule");
          return true;
        }
      }
      break;
    }
        
    case SUBMODULE_HEADER: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (!CurrentModule)
        break;
      
      // FIXME: Be more lazy about this!
      StringRef FileName(BlobStart, BlobLen);
      if (const FileEntry *File = PP.getFileManager().getFile(FileName)) {
        if (std::find(CurrentModule->Headers.begin(), 
                      CurrentModule->Headers.end(), 
                      File) == CurrentModule->Headers.end())
          ModMap.addHeader(CurrentModule, File, false);
      }
      break;      
    }

    case SUBMODULE_EXCLUDED_HEADER: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (!CurrentModule)
        break;
      
      // FIXME: Be more lazy about this!
      StringRef FileName(BlobStart, BlobLen);
      if (const FileEntry *File = PP.getFileManager().getFile(FileName)) {
        if (std::find(CurrentModule->Headers.begin(), 
                      CurrentModule->Headers.end(), 
                      File) == CurrentModule->Headers.end())
          ModMap.addHeader(CurrentModule, File, true);
      }
      break;      
    }

    case SUBMODULE_TOPHEADER: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (!CurrentModule)
        break;

      // FIXME: Be more lazy about this!
      StringRef FileName(BlobStart, BlobLen);
      if (const FileEntry *File = PP.getFileManager().getFile(FileName))
        CurrentModule->TopHeaders.insert(File);
      break;
    }

    case SUBMODULE_UMBRELLA_DIR: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }
      
      if (!CurrentModule)
        break;
      
      StringRef DirName(BlobStart, BlobLen);
      if (const DirectoryEntry *Umbrella
                                  = PP.getFileManager().getDirectory(DirName)) {
        if (!CurrentModule->getUmbrellaDir())
          ModMap.setUmbrellaDir(CurrentModule, Umbrella);
        else if (CurrentModule->getUmbrellaDir() != Umbrella) {
          Error("mismatched umbrella directories in submodule");
          return true;
        }
      }
      break;
    }
        
    case SUBMODULE_METADATA: {
      if (!First) {
        Error("submodule metadata record not at beginning of block");
        return true;
      }
      First = false;
      
      F.BaseSubmoduleID = getTotalNumSubmodules();
      F.LocalNumSubmodules = Record[0];
      unsigned LocalBaseSubmoduleID = Record[1];
      if (F.LocalNumSubmodules > 0) {
        // Introduce the global -> local mapping for submodules within this 
        // module.
        GlobalSubmoduleMap.insert(std::make_pair(getTotalNumSubmodules()+1,&F));
        
        // Introduce the local -> global mapping for submodules within this 
        // module.
        F.SubmoduleRemap.insertOrReplace(
          std::make_pair(LocalBaseSubmoduleID,
                         F.BaseSubmoduleID - LocalBaseSubmoduleID));
        
        SubmodulesLoaded.resize(SubmodulesLoaded.size() + F.LocalNumSubmodules);
      }      
      break;
    }
        
    case SUBMODULE_IMPORTS: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }
      
      if (!CurrentModule)
        break;
      
      for (unsigned Idx = 0; Idx != Record.size(); ++Idx) {
        UnresolvedModuleImportExport Unresolved;
        Unresolved.File = &F;
        Unresolved.Mod = CurrentModule;
        Unresolved.ID = Record[Idx];
        Unresolved.IsImport = true;
        Unresolved.IsWildcard = false;
        UnresolvedModuleImportExports.push_back(Unresolved);
      }
      break;
    }

    case SUBMODULE_EXPORTS: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }
      
      if (!CurrentModule)
        break;
      
      for (unsigned Idx = 0; Idx + 1 < Record.size(); Idx += 2) {
        UnresolvedModuleImportExport Unresolved;
        Unresolved.File = &F;
        Unresolved.Mod = CurrentModule;
        Unresolved.ID = Record[Idx];
        Unresolved.IsImport = false;
        Unresolved.IsWildcard = Record[Idx + 1];
        UnresolvedModuleImportExports.push_back(Unresolved);
      }
      
      // Once we've loaded the set of exports, there's no reason to keep 
      // the parsed, unresolved exports around.
      CurrentModule->UnresolvedExports.clear();
      break;
    }
    case SUBMODULE_REQUIRES: {
      if (First) {
        Error("missing submodule metadata record at beginning of block");
        return true;
      }

      if (!CurrentModule)
        break;

      CurrentModule->addRequirement(StringRef(BlobStart, BlobLen), 
                                    Context.getLangOpts(),
                                    Context.getTargetInfo());
      break;
    }
    }
  }
}

/// \brief Parse the record that corresponds to a LangOptions data
/// structure.
///
/// This routine parses the language options from the AST file and then gives
/// them to the AST listener if one is set.
///
/// \returns true if the listener deems the file unacceptable, false otherwise.
bool ASTReader::ParseLanguageOptions(const RecordData &Record,
                                     bool Complain,
                                     ASTReaderListener &Listener) {
  LangOptions LangOpts;
  unsigned Idx = 0;
#define LANGOPT(Name, Bits, Default, Description) \
  LangOpts.Name = Record[Idx++];
#define ENUM_LANGOPT(Name, Type, Bits, Default, Description) \
  LangOpts.set##Name(static_cast<LangOptions::Type>(Record[Idx++]));
#include "clang/Basic/LangOptions.def"

  ObjCRuntime::Kind runtimeKind = (ObjCRuntime::Kind) Record[Idx++];
  VersionTuple runtimeVersion = ReadVersionTuple(Record, Idx);
  LangOpts.ObjCRuntime = ObjCRuntime(runtimeKind, runtimeVersion);
  
  unsigned Length = Record[Idx++];
  LangOpts.CurrentModule.assign(Record.begin() + Idx, 
                                Record.begin() + Idx + Length);
  return Listener.ReadLanguageOptions(LangOpts, Complain);
}

bool ASTReader::ParseTargetOptions(const RecordData &Record,
                                   bool Complain,
                                   ASTReaderListener &Listener) {
  unsigned Idx = 0;
  TargetOptions TargetOpts;
  TargetOpts.Triple = ReadString(Record, Idx);
  TargetOpts.CPU = ReadString(Record, Idx);
  TargetOpts.ABI = ReadString(Record, Idx);
  TargetOpts.CXXABI = ReadString(Record, Idx);
  TargetOpts.LinkerVersion = ReadString(Record, Idx);
  for (unsigned N = Record[Idx++]; N; --N) {
    TargetOpts.FeaturesAsWritten.push_back(ReadString(Record, Idx));
  }
  for (unsigned N = Record[Idx++]; N; --N) {
    TargetOpts.Features.push_back(ReadString(Record, Idx));
  }

  return Listener.ReadTargetOptions(TargetOpts, Complain);
}

bool ASTReader::ParseDiagnosticOptions(const RecordData &Record, bool Complain,
                                       ASTReaderListener &Listener) {
  DiagnosticOptions DiagOpts;
  unsigned Idx = 0;
#define DIAGOPT(Name, Bits, Default) DiagOpts.Name = Record[Idx++];
#define ENUM_DIAGOPT(Name, Type, Bits, Default) \
  DiagOpts.set##Name(static_cast<Type>(Record[Idx++]));
#include "clang/Basic/DiagnosticOptions.def"

  for (unsigned N = Record[Idx++]; N; --N) {
    DiagOpts.Warnings.push_back(ReadString(Record, Idx));
  }

  return Listener.ReadDiagnosticOptions(DiagOpts, Complain);
}

bool ASTReader::ParseFileSystemOptions(const RecordData &Record, bool Complain,
                                       ASTReaderListener &Listener) {
  FileSystemOptions FSOpts;
  unsigned Idx = 0;
  FSOpts.WorkingDir = ReadString(Record, Idx);
  return Listener.ReadFileSystemOptions(FSOpts, Complain);
}

bool ASTReader::ParseHeaderSearchOptions(const RecordData &Record,
                                         bool Complain,
                                         ASTReaderListener &Listener) {
  HeaderSearchOptions HSOpts;
  unsigned Idx = 0;
  HSOpts.Sysroot = ReadString(Record, Idx);

  // Include entries.
  for (unsigned N = Record[Idx++]; N; --N) {
    std::string Path = ReadString(Record, Idx);
    frontend::IncludeDirGroup Group
      = static_cast<frontend::IncludeDirGroup>(Record[Idx++]);
    bool IsUserSupplied = Record[Idx++];
    bool IsFramework = Record[Idx++];
    bool IgnoreSysRoot = Record[Idx++];
    bool IsInternal = Record[Idx++];
    bool ImplicitExternC = Record[Idx++];
    HSOpts.UserEntries.push_back(
      HeaderSearchOptions::Entry(Path, Group, IsUserSupplied, IsFramework,
                                 IgnoreSysRoot, IsInternal, ImplicitExternC));
  }

  // System header prefixes.
  for (unsigned N = Record[Idx++]; N; --N) {
    std::string Prefix = ReadString(Record, Idx);
    bool IsSystemHeader = Record[Idx++];
    HSOpts.SystemHeaderPrefixes.push_back(
      HeaderSearchOptions::SystemHeaderPrefix(Prefix, IsSystemHeader));
  }

  HSOpts.ResourceDir = ReadString(Record, Idx);
  HSOpts.ModuleCachePath = ReadString(Record, Idx);
  HSOpts.DisableModuleHash = Record[Idx++];
  HSOpts.UseBuiltinIncludes = Record[Idx++];
  HSOpts.UseStandardSystemIncludes = Record[Idx++];
  HSOpts.UseStandardCXXIncludes = Record[Idx++];
  HSOpts.UseLibcxx = Record[Idx++];

  return Listener.ReadHeaderSearchOptions(HSOpts, Complain);
}

bool ASTReader::ParsePreprocessorOptions(const RecordData &Record,
                                         bool Complain,
                                         ASTReaderListener &Listener,
                                         std::string &SuggestedPredefines) {
  PreprocessorOptions PPOpts;
  unsigned Idx = 0;

  // Macro definitions/undefs
  for (unsigned N = Record[Idx++]; N; --N) {
    std::string Macro = ReadString(Record, Idx);
    bool IsUndef = Record[Idx++];
    PPOpts.Macros.push_back(std::make_pair(Macro, IsUndef));
  }

  // Includes
  for (unsigned N = Record[Idx++]; N; --N) {
    PPOpts.Includes.push_back(ReadString(Record, Idx));
  }

  // Macro Includes
  for (unsigned N = Record[Idx++]; N; --N) {
    PPOpts.MacroIncludes.push_back(ReadString(Record, Idx));
  }

  PPOpts.UsePredefines = Record[Idx++];
  PPOpts.ImplicitPCHInclude = ReadString(Record, Idx);
  PPOpts.ImplicitPTHInclude = ReadString(Record, Idx);
  PPOpts.ObjCXXARCStandardLibrary =
    static_cast<ObjCXXARCStandardLibraryKind>(Record[Idx++]);
  SuggestedPredefines.clear();
  return Listener.ReadPreprocessorOptions(PPOpts, Complain,
                                          SuggestedPredefines);
}

std::pair<ModuleFile *, unsigned>
ASTReader::getModulePreprocessedEntity(unsigned GlobalIndex) {
  GlobalPreprocessedEntityMapType::iterator
  I = GlobalPreprocessedEntityMap.find(GlobalIndex);
  assert(I != GlobalPreprocessedEntityMap.end() && 
         "Corrupted global preprocessed entity map");
  ModuleFile *M = I->second;
  unsigned LocalIndex = GlobalIndex - M->BasePreprocessedEntityID;
  return std::make_pair(M, LocalIndex);
}

std::pair<PreprocessingRecord::iterator, PreprocessingRecord::iterator>
ASTReader::getModulePreprocessedEntities(ModuleFile &Mod) const {
  if (PreprocessingRecord *PPRec = PP.getPreprocessingRecord())
    return PPRec->getIteratorsForLoadedRange(Mod.BasePreprocessedEntityID,
                                             Mod.NumPreprocessedEntities);

  return std::make_pair(PreprocessingRecord::iterator(),
                        PreprocessingRecord::iterator());
}

std::pair<ASTReader::ModuleDeclIterator, ASTReader::ModuleDeclIterator>
ASTReader::getModuleFileLevelDecls(ModuleFile &Mod) {
  return std::make_pair(ModuleDeclIterator(this, &Mod, Mod.FileSortedDecls),
                        ModuleDeclIterator(this, &Mod,
                                 Mod.FileSortedDecls + Mod.NumFileSortedDecls));
}

PreprocessedEntity *ASTReader::ReadPreprocessedEntity(unsigned Index) {
  PreprocessedEntityID PPID = Index+1;
  std::pair<ModuleFile *, unsigned> PPInfo = getModulePreprocessedEntity(Index);
  ModuleFile &M = *PPInfo.first;
  unsigned LocalIndex = PPInfo.second;
  const PPEntityOffset &PPOffs = M.PreprocessedEntityOffsets[LocalIndex];

  SavedStreamPosition SavedPosition(M.PreprocessorDetailCursor);  
  M.PreprocessorDetailCursor.JumpToBit(PPOffs.BitOffset);

  unsigned Code = M.PreprocessorDetailCursor.ReadCode();
  switch (Code) {
  case llvm::bitc::END_BLOCK:
    return 0;
    
  case llvm::bitc::ENTER_SUBBLOCK:
    Error("unexpected subblock record in preprocessor detail block");
    return 0;
      
  case llvm::bitc::DEFINE_ABBREV:
    Error("unexpected abbrevation record in preprocessor detail block");
    return 0;
      
  default:
    break;
  }

  if (!PP.getPreprocessingRecord()) {
    Error("no preprocessing record");
    return 0;
  }
  
  // Read the record.
  SourceRange Range(ReadSourceLocation(M, PPOffs.Begin),
                    ReadSourceLocation(M, PPOffs.End));
  PreprocessingRecord &PPRec = *PP.getPreprocessingRecord();
  const char *BlobStart = 0;
  unsigned BlobLen = 0;
  RecordData Record;
  PreprocessorDetailRecordTypes RecType =
    (PreprocessorDetailRecordTypes)M.PreprocessorDetailCursor.ReadRecord(
                                             Code, Record, BlobStart, BlobLen);
  switch (RecType) {
  case PPD_MACRO_EXPANSION: {
    bool isBuiltin = Record[0];
    IdentifierInfo *Name = 0;
    MacroDefinition *Def = 0;
    if (isBuiltin)
      Name = getLocalIdentifier(M, Record[1]);
    else {
      PreprocessedEntityID
          GlobalID = getGlobalPreprocessedEntityID(M, Record[1]);
      Def =cast<MacroDefinition>(PPRec.getLoadedPreprocessedEntity(GlobalID-1));
    }

    MacroExpansion *ME;
    if (isBuiltin)
      ME = new (PPRec) MacroExpansion(Name, Range);
    else
      ME = new (PPRec) MacroExpansion(Def, Range);

    return ME;
  }
      
  case PPD_MACRO_DEFINITION: {
    // Decode the identifier info and then check again; if the macro is
    // still defined and associated with the identifier,
    IdentifierInfo *II = getLocalIdentifier(M, Record[0]);
    MacroDefinition *MD
      = new (PPRec) MacroDefinition(II, Range);

    if (DeserializationListener)
      DeserializationListener->MacroDefinitionRead(PPID, MD);

    return MD;
  }
      
  case PPD_INCLUSION_DIRECTIVE: {
    const char *FullFileNameStart = BlobStart + Record[0];
    StringRef FullFileName(FullFileNameStart, BlobLen - Record[0]);
    const FileEntry *File = 0;
    if (!FullFileName.empty())
      File = PP.getFileManager().getFile(FullFileName);
    
    // FIXME: Stable encoding
    InclusionDirective::InclusionKind Kind
      = static_cast<InclusionDirective::InclusionKind>(Record[2]);
    InclusionDirective *ID
      = new (PPRec) InclusionDirective(PPRec, Kind,
                                       StringRef(BlobStart, Record[0]),
                                       Record[1], Record[3],
                                       File,
                                       Range);
    return ID;
  }
  }

  llvm_unreachable("Invalid PreprocessorDetailRecordTypes");
}

/// \brief \arg SLocMapI points at a chunk of a module that contains no
/// preprocessed entities or the entities it contains are not the ones we are
/// looking for. Find the next module that contains entities and return the ID
/// of the first entry.
PreprocessedEntityID ASTReader::findNextPreprocessedEntity(
                       GlobalSLocOffsetMapType::const_iterator SLocMapI) const {
  ++SLocMapI;
  for (GlobalSLocOffsetMapType::const_iterator
         EndI = GlobalSLocOffsetMap.end(); SLocMapI != EndI; ++SLocMapI) {
    ModuleFile &M = *SLocMapI->second;
    if (M.NumPreprocessedEntities)
      return M.BasePreprocessedEntityID;
  }

  return getTotalNumPreprocessedEntities();
}

namespace {

template <unsigned PPEntityOffset::*PPLoc>
struct PPEntityComp {
  const ASTReader &Reader;
  ModuleFile &M;

  PPEntityComp(const ASTReader &Reader, ModuleFile &M) : Reader(Reader), M(M) { }

  bool operator()(const PPEntityOffset &L, const PPEntityOffset &R) const {
    SourceLocation LHS = getLoc(L);
    SourceLocation RHS = getLoc(R);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  bool operator()(const PPEntityOffset &L, SourceLocation RHS) const {
    SourceLocation LHS = getLoc(L);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  bool operator()(SourceLocation LHS, const PPEntityOffset &R) const {
    SourceLocation RHS = getLoc(R);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  SourceLocation getLoc(const PPEntityOffset &PPE) const {
    return Reader.ReadSourceLocation(M, PPE.*PPLoc);
  }
};

}

/// \brief Returns the first preprocessed entity ID that ends after \arg BLoc.
PreprocessedEntityID
ASTReader::findBeginPreprocessedEntity(SourceLocation BLoc) const {
  if (SourceMgr.isLocalSourceLocation(BLoc))
    return getTotalNumPreprocessedEntities();

  GlobalSLocOffsetMapType::const_iterator
    SLocMapI = GlobalSLocOffsetMap.find(SourceManager::MaxLoadedOffset -
                                        BLoc.getOffset());
  assert(SLocMapI != GlobalSLocOffsetMap.end() &&
         "Corrupted global sloc offset map");

  if (SLocMapI->second->NumPreprocessedEntities == 0)
    return findNextPreprocessedEntity(SLocMapI);

  ModuleFile &M = *SLocMapI->second;
  typedef const PPEntityOffset *pp_iterator;
  pp_iterator pp_begin = M.PreprocessedEntityOffsets;
  pp_iterator pp_end = pp_begin + M.NumPreprocessedEntities;

  size_t Count = M.NumPreprocessedEntities;
  size_t Half;
  pp_iterator First = pp_begin;
  pp_iterator PPI;

  // Do a binary search manually instead of using std::lower_bound because
  // The end locations of entities may be unordered (when a macro expansion
  // is inside another macro argument), but for this case it is not important
  // whether we get the first macro expansion or its containing macro.
  while (Count > 0) {
    Half = Count/2;
    PPI = First;
    std::advance(PPI, Half);
    if (SourceMgr.isBeforeInTranslationUnit(ReadSourceLocation(M, PPI->End),
                                            BLoc)){
      First = PPI;
      ++First;
      Count = Count - Half - 1;
    } else
      Count = Half;
  }

  if (PPI == pp_end)
    return findNextPreprocessedEntity(SLocMapI);

  return M.BasePreprocessedEntityID + (PPI - pp_begin);
}

/// \brief Returns the first preprocessed entity ID that begins after \arg ELoc.
PreprocessedEntityID
ASTReader::findEndPreprocessedEntity(SourceLocation ELoc) const {
  if (SourceMgr.isLocalSourceLocation(ELoc))
    return getTotalNumPreprocessedEntities();

  GlobalSLocOffsetMapType::const_iterator
    SLocMapI = GlobalSLocOffsetMap.find(SourceManager::MaxLoadedOffset -
                                        ELoc.getOffset());
  assert(SLocMapI != GlobalSLocOffsetMap.end() &&
         "Corrupted global sloc offset map");

  if (SLocMapI->second->NumPreprocessedEntities == 0)
    return findNextPreprocessedEntity(SLocMapI);

  ModuleFile &M = *SLocMapI->second;
  typedef const PPEntityOffset *pp_iterator;
  pp_iterator pp_begin = M.PreprocessedEntityOffsets;
  pp_iterator pp_end = pp_begin + M.NumPreprocessedEntities;
  pp_iterator PPI =
      std::upper_bound(pp_begin, pp_end, ELoc,
                       PPEntityComp<&PPEntityOffset::Begin>(*this, M));

  if (PPI == pp_end)
    return findNextPreprocessedEntity(SLocMapI);

  return M.BasePreprocessedEntityID + (PPI - pp_begin);
}

/// \brief Returns a pair of [Begin, End) indices of preallocated
/// preprocessed entities that \arg Range encompasses.
std::pair<unsigned, unsigned>
    ASTReader::findPreprocessedEntitiesInRange(SourceRange Range) {
  if (Range.isInvalid())
    return std::make_pair(0,0);
  assert(!SourceMgr.isBeforeInTranslationUnit(Range.getEnd(),Range.getBegin()));

  PreprocessedEntityID BeginID = findBeginPreprocessedEntity(Range.getBegin());
  PreprocessedEntityID EndID = findEndPreprocessedEntity(Range.getEnd());
  return std::make_pair(BeginID, EndID);
}

/// \brief Optionally returns true or false if the preallocated preprocessed
/// entity with index \arg Index came from file \arg FID.
llvm::Optional<bool> ASTReader::isPreprocessedEntityInFileID(unsigned Index,
                                                             FileID FID) {
  if (FID.isInvalid())
    return false;

  std::pair<ModuleFile *, unsigned> PPInfo = getModulePreprocessedEntity(Index);
  ModuleFile &M = *PPInfo.first;
  unsigned LocalIndex = PPInfo.second;
  const PPEntityOffset &PPOffs = M.PreprocessedEntityOffsets[LocalIndex];
  
  SourceLocation Loc = ReadSourceLocation(M, PPOffs.Begin);
  if (Loc.isInvalid())
    return false;
  
  if (SourceMgr.isInFileID(SourceMgr.getFileLoc(Loc), FID))
    return true;
  else
    return false;
}

namespace {
  /// \brief Visitor used to search for information about a header file.
  class HeaderFileInfoVisitor {
    ASTReader &Reader;
    const FileEntry *FE;
    
    llvm::Optional<HeaderFileInfo> HFI;
    
  public:
    HeaderFileInfoVisitor(ASTReader &Reader, const FileEntry *FE)
      : Reader(Reader), FE(FE) { }
    
    static bool visit(ModuleFile &M, void *UserData) {
      HeaderFileInfoVisitor *This
        = static_cast<HeaderFileInfoVisitor *>(UserData);
      
      HeaderFileInfoTrait Trait(This->Reader, M, 
                                &This->Reader.getPreprocessor().getHeaderSearchInfo(),
                                M.HeaderFileFrameworkStrings,
                                This->FE->getName());
      
      HeaderFileInfoLookupTable *Table
        = static_cast<HeaderFileInfoLookupTable *>(M.HeaderFileInfoTable);
      if (!Table)
        return false;

      // Look in the on-disk hash table for an entry for this file name.
      HeaderFileInfoLookupTable::iterator Pos = Table->find(This->FE->getName(),
                                                            &Trait);
      if (Pos == Table->end())
        return false;

      This->HFI = *Pos;
      return true;
    }
    
    llvm::Optional<HeaderFileInfo> getHeaderFileInfo() const { return HFI; }
  };
}

HeaderFileInfo ASTReader::GetHeaderFileInfo(const FileEntry *FE) {
  HeaderFileInfoVisitor Visitor(*this, FE);
  ModuleMgr.visit(&HeaderFileInfoVisitor::visit, &Visitor);
  if (llvm::Optional<HeaderFileInfo> HFI = Visitor.getHeaderFileInfo()) {
    if (Listener)
      Listener->ReadHeaderFileInfo(*HFI, FE->getUID());
    return *HFI;
  }
  
  return HeaderFileInfo();
}

void ASTReader::ReadPragmaDiagnosticMappings(DiagnosticsEngine &Diag) {
  // FIXME: Make it work properly with modules.
  llvm::SmallVector<DiagnosticsEngine::DiagState *, 32> DiagStates;
  for (ModuleIterator I = ModuleMgr.begin(), E = ModuleMgr.end(); I != E; ++I) {
    ModuleFile &F = *(*I);
    unsigned Idx = 0;
    DiagStates.clear();
    assert(!Diag.DiagStates.empty());
    DiagStates.push_back(&Diag.DiagStates.front()); // the command-line one.
    while (Idx < F.PragmaDiagMappings.size()) {
      SourceLocation Loc = ReadSourceLocation(F, F.PragmaDiagMappings[Idx++]);
      unsigned DiagStateID = F.PragmaDiagMappings[Idx++];
      if (DiagStateID != 0) {
        Diag.DiagStatePoints.push_back(
                    DiagnosticsEngine::DiagStatePoint(DiagStates[DiagStateID-1],
                    FullSourceLoc(Loc, SourceMgr)));
        continue;
      }
      
      assert(DiagStateID == 0);
      // A new DiagState was created here.
      Diag.DiagStates.push_back(*Diag.GetCurDiagState());
      DiagnosticsEngine::DiagState *NewState = &Diag.DiagStates.back();
      DiagStates.push_back(NewState);
      Diag.DiagStatePoints.push_back(
          DiagnosticsEngine::DiagStatePoint(NewState,
                                            FullSourceLoc(Loc, SourceMgr)));
      while (1) {
        assert(Idx < F.PragmaDiagMappings.size() &&
               "Invalid data, didn't find '-1' marking end of diag/map pairs");
        if (Idx >= F.PragmaDiagMappings.size()) {
          break; // Something is messed up but at least avoid infinite loop in
                 // release build.
        }
        unsigned DiagID = F.PragmaDiagMappings[Idx++];
        if (DiagID == (unsigned)-1) {
          break; // no more diag/map pairs for this location.
        }
        diag::Mapping Map = (diag::Mapping)F.PragmaDiagMappings[Idx++];
        DiagnosticMappingInfo MappingInfo = Diag.makeMappingInfo(Map, Loc);
        Diag.GetCurDiagState()->setMappingInfo(DiagID, MappingInfo);
      }
    }
  }
}

/// \brief Get the correct cursor and offset for loading a type.
ASTReader::RecordLocation ASTReader::TypeCursorForIndex(unsigned Index) {
  GlobalTypeMapType::iterator I = GlobalTypeMap.find(Index);
  assert(I != GlobalTypeMap.end() && "Corrupted global type map");
  ModuleFile *M = I->second;
  return RecordLocation(M, M->TypeOffsets[Index - M->BaseTypeIndex]);
}

/// \brief Read and return the type with the given index..
///
/// The index is the type ID, shifted and minus the number of predefs. This
/// routine actually reads the record corresponding to the type at the given
/// location. It is a helper routine for GetType, which deals with reading type
/// IDs.
QualType ASTReader::readTypeRecord(unsigned Index) {
  RecordLocation Loc = TypeCursorForIndex(Index);
  llvm::BitstreamCursor &DeclsCursor = Loc.F->DeclsCursor;

  // Keep track of where we are in the stream, then jump back there
  // after reading this type.
  SavedStreamPosition SavedPosition(DeclsCursor);

  ReadingKindTracker ReadingKind(Read_Type, *this);

  // Note that we are loading a type record.
  Deserializing AType(this);

  unsigned Idx = 0;
  DeclsCursor.JumpToBit(Loc.Offset);
  RecordData Record;
  unsigned Code = DeclsCursor.ReadCode();
  switch ((TypeCode)DeclsCursor.ReadRecord(Code, Record)) {
  case TYPE_EXT_QUAL: {
    if (Record.size() != 2) {
      Error("Incorrect encoding of extended qualifier type");
      return QualType();
    }
    QualType Base = readType(*Loc.F, Record, Idx);
    Qualifiers Quals = Qualifiers::fromOpaqueValue(Record[Idx++]);
    return Context.getQualifiedType(Base, Quals);
  }

  case TYPE_COMPLEX: {
    if (Record.size() != 1) {
      Error("Incorrect encoding of complex type");
      return QualType();
    }
    QualType ElemType = readType(*Loc.F, Record, Idx);
    return Context.getComplexType(ElemType);
  }

  case TYPE_POINTER: {
    if (Record.size() != 1) {
      Error("Incorrect encoding of pointer type");
      return QualType();
    }
    QualType PointeeType = readType(*Loc.F, Record, Idx);
    return Context.getPointerType(PointeeType);
  }

  case TYPE_BLOCK_POINTER: {
    if (Record.size() != 1) {
      Error("Incorrect encoding of block pointer type");
      return QualType();
    }
    QualType PointeeType = readType(*Loc.F, Record, Idx);
    return Context.getBlockPointerType(PointeeType);
  }

  case TYPE_LVALUE_REFERENCE: {
    if (Record.size() != 2) {
      Error("Incorrect encoding of lvalue reference type");
      return QualType();
    }
    QualType PointeeType = readType(*Loc.F, Record, Idx);
    return Context.getLValueReferenceType(PointeeType, Record[1]);
  }

  case TYPE_RVALUE_REFERENCE: {
    if (Record.size() != 1) {
      Error("Incorrect encoding of rvalue reference type");
      return QualType();
    }
    QualType PointeeType = readType(*Loc.F, Record, Idx);
    return Context.getRValueReferenceType(PointeeType);
  }

  case TYPE_MEMBER_POINTER: {
    if (Record.size() != 2) {
      Error("Incorrect encoding of member pointer type");
      return QualType();
    }
    QualType PointeeType = readType(*Loc.F, Record, Idx);
    QualType ClassType = readType(*Loc.F, Record, Idx);
    if (PointeeType.isNull() || ClassType.isNull())
      return QualType();
    
    return Context.getMemberPointerType(PointeeType, ClassType.getTypePtr());
  }

  case TYPE_CONSTANT_ARRAY: {
    QualType ElementType = readType(*Loc.F, Record, Idx);
    ArrayType::ArraySizeModifier ASM = (ArrayType::ArraySizeModifier)Record[1];
    unsigned IndexTypeQuals = Record[2];
    unsigned Idx = 3;
    llvm::APInt Size = ReadAPInt(Record, Idx);
    return Context.getConstantArrayType(ElementType, Size,
                                         ASM, IndexTypeQuals);
  }

  case TYPE_INCOMPLETE_ARRAY: {
    QualType ElementType = readType(*Loc.F, Record, Idx);
    ArrayType::ArraySizeModifier ASM = (ArrayType::ArraySizeModifier)Record[1];
    unsigned IndexTypeQuals = Record[2];
    return Context.getIncompleteArrayType(ElementType, ASM, IndexTypeQuals);
  }

  case TYPE_VARIABLE_ARRAY: {
    QualType ElementType = readType(*Loc.F, Record, Idx);
    ArrayType::ArraySizeModifier ASM = (ArrayType::ArraySizeModifier)Record[1];
    unsigned IndexTypeQuals = Record[2];
    SourceLocation LBLoc = ReadSourceLocation(*Loc.F, Record[3]);
    SourceLocation RBLoc = ReadSourceLocation(*Loc.F, Record[4]);
    return Context.getVariableArrayType(ElementType, ReadExpr(*Loc.F),
                                         ASM, IndexTypeQuals,
                                         SourceRange(LBLoc, RBLoc));
  }

  case TYPE_VECTOR: {
    if (Record.size() != 3) {
      Error("incorrect encoding of vector type in AST file");
      return QualType();
    }

    QualType ElementType = readType(*Loc.F, Record, Idx);
    unsigned NumElements = Record[1];
    unsigned VecKind = Record[2];
    return Context.getVectorType(ElementType, NumElements,
                                  (VectorType::VectorKind)VecKind);
  }

  case TYPE_EXT_VECTOR: {
    if (Record.size() != 3) {
      Error("incorrect encoding of extended vector type in AST file");
      return QualType();
    }

    QualType ElementType = readType(*Loc.F, Record, Idx);
    unsigned NumElements = Record[1];
    return Context.getExtVectorType(ElementType, NumElements);
  }

  case TYPE_FUNCTION_NO_PROTO: {
    if (Record.size() != 6) {
      Error("incorrect encoding of no-proto function type");
      return QualType();
    }
    QualType ResultType = readType(*Loc.F, Record, Idx);
    FunctionType::ExtInfo Info(Record[1], Record[2], Record[3],
                               (CallingConv)Record[4], Record[5]);
    return Context.getFunctionNoProtoType(ResultType, Info);
  }

  case TYPE_FUNCTION_PROTO: {
    QualType ResultType = readType(*Loc.F, Record, Idx);

    FunctionProtoType::ExtProtoInfo EPI;
    EPI.ExtInfo = FunctionType::ExtInfo(/*noreturn*/ Record[1],
                                        /*hasregparm*/ Record[2],
                                        /*regparm*/ Record[3],
                                        static_cast<CallingConv>(Record[4]),
                                        /*produces*/ Record[5]);

    unsigned Idx = 6;
    unsigned NumParams = Record[Idx++];
    SmallVector<QualType, 16> ParamTypes;
    for (unsigned I = 0; I != NumParams; ++I)
      ParamTypes.push_back(readType(*Loc.F, Record, Idx));

    EPI.Variadic = Record[Idx++];
    EPI.HasTrailingReturn = Record[Idx++];
    EPI.TypeQuals = Record[Idx++];
    EPI.RefQualifier = static_cast<RefQualifierKind>(Record[Idx++]);
    ExceptionSpecificationType EST =
        static_cast<ExceptionSpecificationType>(Record[Idx++]);
    EPI.ExceptionSpecType = EST;
    SmallVector<QualType, 2> Exceptions;
    if (EST == EST_Dynamic) {
      EPI.NumExceptions = Record[Idx++];
      for (unsigned I = 0; I != EPI.NumExceptions; ++I)
        Exceptions.push_back(readType(*Loc.F, Record, Idx));
      EPI.Exceptions = Exceptions.data();
    } else if (EST == EST_ComputedNoexcept) {
      EPI.NoexceptExpr = ReadExpr(*Loc.F);
    } else if (EST == EST_Uninstantiated) {
      EPI.ExceptionSpecDecl = ReadDeclAs<FunctionDecl>(*Loc.F, Record, Idx);
      EPI.ExceptionSpecTemplate = ReadDeclAs<FunctionDecl>(*Loc.F, Record, Idx);
    } else if (EST == EST_Unevaluated) {
      EPI.ExceptionSpecDecl = ReadDeclAs<FunctionDecl>(*Loc.F, Record, Idx);
    }
    return Context.getFunctionType(ResultType, ParamTypes.data(), NumParams,
                                    EPI);
  }

  case TYPE_UNRESOLVED_USING: {
    unsigned Idx = 0;
    return Context.getTypeDeclType(
                  ReadDeclAs<UnresolvedUsingTypenameDecl>(*Loc.F, Record, Idx));
  }
      
  case TYPE_TYPEDEF: {
    if (Record.size() != 2) {
      Error("incorrect encoding of typedef type");
      return QualType();
    }
    unsigned Idx = 0;
    TypedefNameDecl *Decl = ReadDeclAs<TypedefNameDecl>(*Loc.F, Record, Idx);
    QualType Canonical = readType(*Loc.F, Record, Idx);
    if (!Canonical.isNull())
      Canonical = Context.getCanonicalType(Canonical);
    return Context.getTypedefType(Decl, Canonical);
  }

  case TYPE_TYPEOF_EXPR:
    return Context.getTypeOfExprType(ReadExpr(*Loc.F));

  case TYPE_TYPEOF: {
    if (Record.size() != 1) {
      Error("incorrect encoding of typeof(type) in AST file");
      return QualType();
    }
    QualType UnderlyingType = readType(*Loc.F, Record, Idx);
    return Context.getTypeOfType(UnderlyingType);
  }

  case TYPE_DECLTYPE: {
    QualType UnderlyingType = readType(*Loc.F, Record, Idx);
    return Context.getDecltypeType(ReadExpr(*Loc.F), UnderlyingType);
  }

  case TYPE_UNARY_TRANSFORM: {
    QualType BaseType = readType(*Loc.F, Record, Idx);
    QualType UnderlyingType = readType(*Loc.F, Record, Idx);
    UnaryTransformType::UTTKind UKind = (UnaryTransformType::UTTKind)Record[2];
    return Context.getUnaryTransformType(BaseType, UnderlyingType, UKind);
  }

  case TYPE_AUTO:
    return Context.getAutoType(readType(*Loc.F, Record, Idx));

  case TYPE_RECORD: {
    if (Record.size() != 2) {
      Error("incorrect encoding of record type");
      return QualType();
    }
    unsigned Idx = 0;
    bool IsDependent = Record[Idx++];
    RecordDecl *RD = ReadDeclAs<RecordDecl>(*Loc.F, Record, Idx);
    RD = cast_or_null<RecordDecl>(RD->getCanonicalDecl());
    QualType T = Context.getRecordType(RD);
    const_cast<Type*>(T.getTypePtr())->setDependent(IsDependent);
    return T;
  }

  case TYPE_ENUM: {
    if (Record.size() != 2) {
      Error("incorrect encoding of enum type");
      return QualType();
    }
    unsigned Idx = 0;
    bool IsDependent = Record[Idx++];
    QualType T
      = Context.getEnumType(ReadDeclAs<EnumDecl>(*Loc.F, Record, Idx));
    const_cast<Type*>(T.getTypePtr())->setDependent(IsDependent);
    return T;
  }

  case TYPE_ATTRIBUTED: {
    if (Record.size() != 3) {
      Error("incorrect encoding of attributed type");
      return QualType();
    }
    QualType modifiedType = readType(*Loc.F, Record, Idx);
    QualType equivalentType = readType(*Loc.F, Record, Idx);
    AttributedType::Kind kind = static_cast<AttributedType::Kind>(Record[2]);
    return Context.getAttributedType(kind, modifiedType, equivalentType);
  }

  case TYPE_PAREN: {
    if (Record.size() != 1) {
      Error("incorrect encoding of paren type");
      return QualType();
    }
    QualType InnerType = readType(*Loc.F, Record, Idx);
    return Context.getParenType(InnerType);
  }

  case TYPE_PACK_EXPANSION: {
    if (Record.size() != 2) {
      Error("incorrect encoding of pack expansion type");
      return QualType();
    }
    QualType Pattern = readType(*Loc.F, Record, Idx);
    if (Pattern.isNull())
      return QualType();
    llvm::Optional<unsigned> NumExpansions;
    if (Record[1])
      NumExpansions = Record[1] - 1;
    return Context.getPackExpansionType(Pattern, NumExpansions);
  }

  case TYPE_ELABORATED: {
    unsigned Idx = 0;
    ElaboratedTypeKeyword Keyword = (ElaboratedTypeKeyword)Record[Idx++];
    NestedNameSpecifier *NNS = ReadNestedNameSpecifier(*Loc.F, Record, Idx);
    QualType NamedType = readType(*Loc.F, Record, Idx);
    return Context.getElaboratedType(Keyword, NNS, NamedType);
  }

  case TYPE_OBJC_INTERFACE: {
    unsigned Idx = 0;
    ObjCInterfaceDecl *ItfD
      = ReadDeclAs<ObjCInterfaceDecl>(*Loc.F, Record, Idx);
    return Context.getObjCInterfaceType(ItfD->getCanonicalDecl());
  }

  case TYPE_OBJC_OBJECT: {
    unsigned Idx = 0;
    QualType Base = readType(*Loc.F, Record, Idx);
    unsigned NumProtos = Record[Idx++];
    SmallVector<ObjCProtocolDecl*, 4> Protos;
    for (unsigned I = 0; I != NumProtos; ++I)
      Protos.push_back(ReadDeclAs<ObjCProtocolDecl>(*Loc.F, Record, Idx));
    return Context.getObjCObjectType(Base, Protos.data(), NumProtos);
  }

  case TYPE_OBJC_OBJECT_POINTER: {
    unsigned Idx = 0;
    QualType Pointee = readType(*Loc.F, Record, Idx);
    return Context.getObjCObjectPointerType(Pointee);
  }

  case TYPE_SUBST_TEMPLATE_TYPE_PARM: {
    unsigned Idx = 0;
    QualType Parm = readType(*Loc.F, Record, Idx);
    QualType Replacement = readType(*Loc.F, Record, Idx);
    return
      Context.getSubstTemplateTypeParmType(cast<TemplateTypeParmType>(Parm),
                                            Replacement);
  }

  case TYPE_SUBST_TEMPLATE_TYPE_PARM_PACK: {
    unsigned Idx = 0;
    QualType Parm = readType(*Loc.F, Record, Idx);
    TemplateArgument ArgPack = ReadTemplateArgument(*Loc.F, Record, Idx);
    return Context.getSubstTemplateTypeParmPackType(
                                               cast<TemplateTypeParmType>(Parm),
                                                     ArgPack);
  }

  case TYPE_INJECTED_CLASS_NAME: {
    CXXRecordDecl *D = ReadDeclAs<CXXRecordDecl>(*Loc.F, Record, Idx);
    QualType TST = readType(*Loc.F, Record, Idx); // probably derivable
    // FIXME: ASTContext::getInjectedClassNameType is not currently suitable
    // for AST reading, too much interdependencies.
    return
      QualType(new (Context, TypeAlignment) InjectedClassNameType(D, TST), 0);
  }

  case TYPE_TEMPLATE_TYPE_PARM: {
    unsigned Idx = 0;
    unsigned Depth = Record[Idx++];
    unsigned Index = Record[Idx++];
    bool Pack = Record[Idx++];
    TemplateTypeParmDecl *D
      = ReadDeclAs<TemplateTypeParmDecl>(*Loc.F, Record, Idx);
    return Context.getTemplateTypeParmType(Depth, Index, Pack, D);
  }

  case TYPE_DEPENDENT_NAME: {
    unsigned Idx = 0;
    ElaboratedTypeKeyword Keyword = (ElaboratedTypeKeyword)Record[Idx++];
    NestedNameSpecifier *NNS = ReadNestedNameSpecifier(*Loc.F, Record, Idx);
    const IdentifierInfo *Name = this->GetIdentifierInfo(*Loc.F, Record, Idx);
    QualType Canon = readType(*Loc.F, Record, Idx);
    if (!Canon.isNull())
      Canon = Context.getCanonicalType(Canon);
    return Context.getDependentNameType(Keyword, NNS, Name, Canon);
  }

  case TYPE_DEPENDENT_TEMPLATE_SPECIALIZATION: {
    unsigned Idx = 0;
    ElaboratedTypeKeyword Keyword = (ElaboratedTypeKeyword)Record[Idx++];
    NestedNameSpecifier *NNS = ReadNestedNameSpecifier(*Loc.F, Record, Idx);
    const IdentifierInfo *Name = this->GetIdentifierInfo(*Loc.F, Record, Idx);
    unsigned NumArgs = Record[Idx++];
    SmallVector<TemplateArgument, 8> Args;
    Args.reserve(NumArgs);
    while (NumArgs--)
      Args.push_back(ReadTemplateArgument(*Loc.F, Record, Idx));
    return Context.getDependentTemplateSpecializationType(Keyword, NNS, Name,
                                                      Args.size(), Args.data());
  }

  case TYPE_DEPENDENT_SIZED_ARRAY: {
    unsigned Idx = 0;

    // ArrayType
    QualType ElementType = readType(*Loc.F, Record, Idx);
    ArrayType::ArraySizeModifier ASM
      = (ArrayType::ArraySizeModifier)Record[Idx++];
    unsigned IndexTypeQuals = Record[Idx++];

    // DependentSizedArrayType
    Expr *NumElts = ReadExpr(*Loc.F);
    SourceRange Brackets = ReadSourceRange(*Loc.F, Record, Idx);

    return Context.getDependentSizedArrayType(ElementType, NumElts, ASM,
                                               IndexTypeQuals, Brackets);
  }

  case TYPE_TEMPLATE_SPECIALIZATION: {
    unsigned Idx = 0;
    bool IsDependent = Record[Idx++];
    TemplateName Name = ReadTemplateName(*Loc.F, Record, Idx);
    SmallVector<TemplateArgument, 8> Args;
    ReadTemplateArgumentList(Args, *Loc.F, Record, Idx);
    QualType Underlying = readType(*Loc.F, Record, Idx);
    QualType T;
    if (Underlying.isNull())
      T = Context.getCanonicalTemplateSpecializationType(Name, Args.data(),
                                                          Args.size());
    else
      T = Context.getTemplateSpecializationType(Name, Args.data(),
                                                 Args.size(), Underlying);
    const_cast<Type*>(T.getTypePtr())->setDependent(IsDependent);
    return T;
  }

  case TYPE_ATOMIC: {
    if (Record.size() != 1) {
      Error("Incorrect encoding of atomic type");
      return QualType();
    }
    QualType ValueType = readType(*Loc.F, Record, Idx);
    return Context.getAtomicType(ValueType);
  }
  }
  llvm_unreachable("Invalid TypeCode!");
}

class clang::TypeLocReader : public TypeLocVisitor<TypeLocReader> {
  ASTReader &Reader;
  ModuleFile &F;
  const ASTReader::RecordData &Record;
  unsigned &Idx;

  SourceLocation ReadSourceLocation(const ASTReader::RecordData &R,
                                    unsigned &I) {
    return Reader.ReadSourceLocation(F, R, I);
  }

  template<typename T>
  T *ReadDeclAs(const ASTReader::RecordData &Record, unsigned &Idx) {
    return Reader.ReadDeclAs<T>(F, Record, Idx);
  }
  
public:
  TypeLocReader(ASTReader &Reader, ModuleFile &F,
                const ASTReader::RecordData &Record, unsigned &Idx)
    : Reader(Reader), F(F), Record(Record), Idx(Idx)
  { }

  // We want compile-time assurance that we've enumerated all of
  // these, so unfortunately we have to declare them first, then
  // define them out-of-line.
#define ABSTRACT_TYPELOC(CLASS, PARENT)
#define TYPELOC(CLASS, PARENT) \
  void Visit##CLASS##TypeLoc(CLASS##TypeLoc TyLoc);
#include "clang/AST/TypeLocNodes.def"

  void VisitFunctionTypeLoc(FunctionTypeLoc);
  void VisitArrayTypeLoc(ArrayTypeLoc);
};

void TypeLocReader::VisitQualifiedTypeLoc(QualifiedTypeLoc TL) {
  // nothing to do
}
void TypeLocReader::VisitBuiltinTypeLoc(BuiltinTypeLoc TL) {
  TL.setBuiltinLoc(ReadSourceLocation(Record, Idx));
  if (TL.needsExtraLocalData()) {
    TL.setWrittenTypeSpec(static_cast<DeclSpec::TST>(Record[Idx++]));
    TL.setWrittenSignSpec(static_cast<DeclSpec::TSS>(Record[Idx++]));
    TL.setWrittenWidthSpec(static_cast<DeclSpec::TSW>(Record[Idx++]));
    TL.setModeAttr(Record[Idx++]);
  }
}
void TypeLocReader::VisitComplexTypeLoc(ComplexTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitPointerTypeLoc(PointerTypeLoc TL) {
  TL.setStarLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitBlockPointerTypeLoc(BlockPointerTypeLoc TL) {
  TL.setCaretLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitLValueReferenceTypeLoc(LValueReferenceTypeLoc TL) {
  TL.setAmpLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitRValueReferenceTypeLoc(RValueReferenceTypeLoc TL) {
  TL.setAmpAmpLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitMemberPointerTypeLoc(MemberPointerTypeLoc TL) {
  TL.setStarLoc(ReadSourceLocation(Record, Idx));
  TL.setClassTInfo(Reader.GetTypeSourceInfo(F, Record, Idx));
}
void TypeLocReader::VisitArrayTypeLoc(ArrayTypeLoc TL) {
  TL.setLBracketLoc(ReadSourceLocation(Record, Idx));
  TL.setRBracketLoc(ReadSourceLocation(Record, Idx));
  if (Record[Idx++])
    TL.setSizeExpr(Reader.ReadExpr(F));
  else
    TL.setSizeExpr(0);
}
void TypeLocReader::VisitConstantArrayTypeLoc(ConstantArrayTypeLoc TL) {
  VisitArrayTypeLoc(TL);
}
void TypeLocReader::VisitIncompleteArrayTypeLoc(IncompleteArrayTypeLoc TL) {
  VisitArrayTypeLoc(TL);
}
void TypeLocReader::VisitVariableArrayTypeLoc(VariableArrayTypeLoc TL) {
  VisitArrayTypeLoc(TL);
}
void TypeLocReader::VisitDependentSizedArrayTypeLoc(
                                            DependentSizedArrayTypeLoc TL) {
  VisitArrayTypeLoc(TL);
}
void TypeLocReader::VisitDependentSizedExtVectorTypeLoc(
                                        DependentSizedExtVectorTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitVectorTypeLoc(VectorTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitExtVectorTypeLoc(ExtVectorTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitFunctionTypeLoc(FunctionTypeLoc TL) {
  TL.setLocalRangeBegin(ReadSourceLocation(Record, Idx));
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
  TL.setLocalRangeEnd(ReadSourceLocation(Record, Idx));
  for (unsigned i = 0, e = TL.getNumArgs(); i != e; ++i) {
    TL.setArg(i, ReadDeclAs<ParmVarDecl>(Record, Idx));
  }
}
void TypeLocReader::VisitFunctionProtoTypeLoc(FunctionProtoTypeLoc TL) {
  VisitFunctionTypeLoc(TL);
}
void TypeLocReader::VisitFunctionNoProtoTypeLoc(FunctionNoProtoTypeLoc TL) {
  VisitFunctionTypeLoc(TL);
}
void TypeLocReader::VisitUnresolvedUsingTypeLoc(UnresolvedUsingTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitTypedefTypeLoc(TypedefTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitTypeOfExprTypeLoc(TypeOfExprTypeLoc TL) {
  TL.setTypeofLoc(ReadSourceLocation(Record, Idx));
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitTypeOfTypeLoc(TypeOfTypeLoc TL) {
  TL.setTypeofLoc(ReadSourceLocation(Record, Idx));
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
  TL.setUnderlyingTInfo(Reader.GetTypeSourceInfo(F, Record, Idx));
}
void TypeLocReader::VisitDecltypeTypeLoc(DecltypeTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitUnaryTransformTypeLoc(UnaryTransformTypeLoc TL) {
  TL.setKWLoc(ReadSourceLocation(Record, Idx));
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
  TL.setUnderlyingTInfo(Reader.GetTypeSourceInfo(F, Record, Idx));
}
void TypeLocReader::VisitAutoTypeLoc(AutoTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitRecordTypeLoc(RecordTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitEnumTypeLoc(EnumTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitAttributedTypeLoc(AttributedTypeLoc TL) {
  TL.setAttrNameLoc(ReadSourceLocation(Record, Idx));
  if (TL.hasAttrOperand()) {
    SourceRange range;
    range.setBegin(ReadSourceLocation(Record, Idx));
    range.setEnd(ReadSourceLocation(Record, Idx));
    TL.setAttrOperandParensRange(range);
  }
  if (TL.hasAttrExprOperand()) {
    if (Record[Idx++])
      TL.setAttrExprOperand(Reader.ReadExpr(F));
    else
      TL.setAttrExprOperand(0);
  } else if (TL.hasAttrEnumOperand())
    TL.setAttrEnumOperandLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitTemplateTypeParmTypeLoc(TemplateTypeParmTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitSubstTemplateTypeParmTypeLoc(
                                            SubstTemplateTypeParmTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitSubstTemplateTypeParmPackTypeLoc(
                                          SubstTemplateTypeParmPackTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitTemplateSpecializationTypeLoc(
                                           TemplateSpecializationTypeLoc TL) {
  TL.setTemplateKeywordLoc(ReadSourceLocation(Record, Idx));
  TL.setTemplateNameLoc(ReadSourceLocation(Record, Idx));
  TL.setLAngleLoc(ReadSourceLocation(Record, Idx));
  TL.setRAngleLoc(ReadSourceLocation(Record, Idx));
  for (unsigned i = 0, e = TL.getNumArgs(); i != e; ++i)
    TL.setArgLocInfo(i,
        Reader.GetTemplateArgumentLocInfo(F,
                                          TL.getTypePtr()->getArg(i).getKind(),
                                          Record, Idx));
}
void TypeLocReader::VisitParenTypeLoc(ParenTypeLoc TL) {
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitElaboratedTypeLoc(ElaboratedTypeLoc TL) {
  TL.setElaboratedKeywordLoc(ReadSourceLocation(Record, Idx));
  TL.setQualifierLoc(Reader.ReadNestedNameSpecifierLoc(F, Record, Idx));
}
void TypeLocReader::VisitInjectedClassNameTypeLoc(InjectedClassNameTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitDependentNameTypeLoc(DependentNameTypeLoc TL) {
  TL.setElaboratedKeywordLoc(ReadSourceLocation(Record, Idx));
  TL.setQualifierLoc(Reader.ReadNestedNameSpecifierLoc(F, Record, Idx));
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitDependentTemplateSpecializationTypeLoc(
       DependentTemplateSpecializationTypeLoc TL) {
  TL.setElaboratedKeywordLoc(ReadSourceLocation(Record, Idx));
  TL.setQualifierLoc(Reader.ReadNestedNameSpecifierLoc(F, Record, Idx));
  TL.setTemplateKeywordLoc(ReadSourceLocation(Record, Idx));
  TL.setTemplateNameLoc(ReadSourceLocation(Record, Idx));
  TL.setLAngleLoc(ReadSourceLocation(Record, Idx));
  TL.setRAngleLoc(ReadSourceLocation(Record, Idx));
  for (unsigned I = 0, E = TL.getNumArgs(); I != E; ++I)
    TL.setArgLocInfo(I,
        Reader.GetTemplateArgumentLocInfo(F,
                                          TL.getTypePtr()->getArg(I).getKind(),
                                          Record, Idx));
}
void TypeLocReader::VisitPackExpansionTypeLoc(PackExpansionTypeLoc TL) {
  TL.setEllipsisLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitObjCInterfaceTypeLoc(ObjCInterfaceTypeLoc TL) {
  TL.setNameLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitObjCObjectTypeLoc(ObjCObjectTypeLoc TL) {
  TL.setHasBaseTypeAsWritten(Record[Idx++]);
  TL.setLAngleLoc(ReadSourceLocation(Record, Idx));
  TL.setRAngleLoc(ReadSourceLocation(Record, Idx));
  for (unsigned i = 0, e = TL.getNumProtocols(); i != e; ++i)
    TL.setProtocolLoc(i, ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitObjCObjectPointerTypeLoc(ObjCObjectPointerTypeLoc TL) {
  TL.setStarLoc(ReadSourceLocation(Record, Idx));
}
void TypeLocReader::VisitAtomicTypeLoc(AtomicTypeLoc TL) {
  TL.setKWLoc(ReadSourceLocation(Record, Idx));
  TL.setLParenLoc(ReadSourceLocation(Record, Idx));
  TL.setRParenLoc(ReadSourceLocation(Record, Idx));
}

TypeSourceInfo *ASTReader::GetTypeSourceInfo(ModuleFile &F,
                                             const RecordData &Record,
                                             unsigned &Idx) {
  QualType InfoTy = readType(F, Record, Idx);
  if (InfoTy.isNull())
    return 0;

  TypeSourceInfo *TInfo = getContext().CreateTypeSourceInfo(InfoTy);
  TypeLocReader TLR(*this, F, Record, Idx);
  for (TypeLoc TL = TInfo->getTypeLoc(); !TL.isNull(); TL = TL.getNextTypeLoc())
    TLR.Visit(TL);
  return TInfo;
}

QualType ASTReader::GetType(TypeID ID) {
  unsigned FastQuals = ID & Qualifiers::FastMask;
  unsigned Index = ID >> Qualifiers::FastWidth;

  if (Index < NUM_PREDEF_TYPE_IDS) {
    QualType T;
    switch ((PredefinedTypeIDs)Index) {
    case PREDEF_TYPE_NULL_ID: return QualType();
    case PREDEF_TYPE_VOID_ID: T = Context.VoidTy; break;
    case PREDEF_TYPE_BOOL_ID: T = Context.BoolTy; break;

    case PREDEF_TYPE_CHAR_U_ID:
    case PREDEF_TYPE_CHAR_S_ID:
      // FIXME: Check that the signedness of CharTy is correct!
      T = Context.CharTy;
      break;

    case PREDEF_TYPE_UCHAR_ID:      T = Context.UnsignedCharTy;     break;
    case PREDEF_TYPE_USHORT_ID:     T = Context.UnsignedShortTy;    break;
    case PREDEF_TYPE_UINT_ID:       T = Context.UnsignedIntTy;      break;
    case PREDEF_TYPE_ULONG_ID:      T = Context.UnsignedLongTy;     break;
    case PREDEF_TYPE_ULONGLONG_ID:  T = Context.UnsignedLongLongTy; break;
    case PREDEF_TYPE_UINT128_ID:    T = Context.UnsignedInt128Ty;   break;
    case PREDEF_TYPE_SCHAR_ID:      T = Context.SignedCharTy;       break;
    case PREDEF_TYPE_WCHAR_ID:      T = Context.WCharTy;            break;
    case PREDEF_TYPE_SHORT_ID:      T = Context.ShortTy;            break;
    case PREDEF_TYPE_INT_ID:        T = Context.IntTy;              break;
    case PREDEF_TYPE_LONG_ID:       T = Context.LongTy;             break;
    case PREDEF_TYPE_LONGLONG_ID:   T = Context.LongLongTy;         break;
    case PREDEF_TYPE_INT128_ID:     T = Context.Int128Ty;           break;
    case PREDEF_TYPE_HALF_ID:       T = Context.HalfTy;             break;
    case PREDEF_TYPE_FLOAT_ID:      T = Context.FloatTy;            break;
    case PREDEF_TYPE_DOUBLE_ID:     T = Context.DoubleTy;           break;
    case PREDEF_TYPE_LONGDOUBLE_ID: T = Context.LongDoubleTy;       break;
    case PREDEF_TYPE_OVERLOAD_ID:   T = Context.OverloadTy;         break;
    case PREDEF_TYPE_BOUND_MEMBER:  T = Context.BoundMemberTy;      break;
    case PREDEF_TYPE_PSEUDO_OBJECT: T = Context.PseudoObjectTy;     break;
    case PREDEF_TYPE_DEPENDENT_ID:  T = Context.DependentTy;        break;
    case PREDEF_TYPE_UNKNOWN_ANY:   T = Context.UnknownAnyTy;       break;
    case PREDEF_TYPE_NULLPTR_ID:    T = Context.NullPtrTy;          break;
    case PREDEF_TYPE_CHAR16_ID:     T = Context.Char16Ty;           break;
    case PREDEF_TYPE_CHAR32_ID:     T = Context.Char32Ty;           break;
    case PREDEF_TYPE_OBJC_ID:       T = Context.ObjCBuiltinIdTy;    break;
    case PREDEF_TYPE_OBJC_CLASS:    T = Context.ObjCBuiltinClassTy; break;
    case PREDEF_TYPE_OBJC_SEL:      T = Context.ObjCBuiltinSelTy;   break;
    case PREDEF_TYPE_AUTO_DEDUCT:   T = Context.getAutoDeductType(); break;
        
    case PREDEF_TYPE_AUTO_RREF_DEDUCT: 
      T = Context.getAutoRRefDeductType(); 
      break;

    case PREDEF_TYPE_ARC_UNBRIDGED_CAST:
      T = Context.ARCUnbridgedCastTy;
      break;

    case PREDEF_TYPE_VA_LIST_TAG:
      T = Context.getVaListTagType();
      break;

    case PREDEF_TYPE_BUILTIN_FN:
      T = Context.BuiltinFnTy;
      break;
    }

    assert(!T.isNull() && "Unknown predefined type");
    return T.withFastQualifiers(FastQuals);
  }

  Index -= NUM_PREDEF_TYPE_IDS;
  assert(Index < TypesLoaded.size() && "Type index out-of-range");
  if (TypesLoaded[Index].isNull()) {
    TypesLoaded[Index] = readTypeRecord(Index);
    if (TypesLoaded[Index].isNull())
      return QualType();

    TypesLoaded[Index]->setFromAST();
    if (DeserializationListener)
      DeserializationListener->TypeRead(TypeIdx::fromTypeID(ID),
                                        TypesLoaded[Index]);
  }

  return TypesLoaded[Index].withFastQualifiers(FastQuals);
}

QualType ASTReader::getLocalType(ModuleFile &F, unsigned LocalID) {
  return GetType(getGlobalTypeID(F, LocalID));
}

serialization::TypeID 
ASTReader::getGlobalTypeID(ModuleFile &F, unsigned LocalID) const {
  unsigned FastQuals = LocalID & Qualifiers::FastMask;
  unsigned LocalIndex = LocalID >> Qualifiers::FastWidth;
  
  if (LocalIndex < NUM_PREDEF_TYPE_IDS)
    return LocalID;

  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = F.TypeRemap.find(LocalIndex - NUM_PREDEF_TYPE_IDS);
  assert(I != F.TypeRemap.end() && "Invalid index into type index remap");
  
  unsigned GlobalIndex = LocalIndex + I->second;
  return (GlobalIndex << Qualifiers::FastWidth) | FastQuals;
}

TemplateArgumentLocInfo
ASTReader::GetTemplateArgumentLocInfo(ModuleFile &F,
                                      TemplateArgument::ArgKind Kind,
                                      const RecordData &Record,
                                      unsigned &Index) {
  switch (Kind) {
  case TemplateArgument::Expression:
    return ReadExpr(F);
  case TemplateArgument::Type:
    return GetTypeSourceInfo(F, Record, Index);
  case TemplateArgument::Template: {
    NestedNameSpecifierLoc QualifierLoc = ReadNestedNameSpecifierLoc(F, Record, 
                                                                     Index);
    SourceLocation TemplateNameLoc = ReadSourceLocation(F, Record, Index);
    return TemplateArgumentLocInfo(QualifierLoc, TemplateNameLoc,
                                   SourceLocation());
  }
  case TemplateArgument::TemplateExpansion: {
    NestedNameSpecifierLoc QualifierLoc = ReadNestedNameSpecifierLoc(F, Record, 
                                                                     Index);
    SourceLocation TemplateNameLoc = ReadSourceLocation(F, Record, Index);
    SourceLocation EllipsisLoc = ReadSourceLocation(F, Record, Index);
    return TemplateArgumentLocInfo(QualifierLoc, TemplateNameLoc, 
                                   EllipsisLoc);
  }
  case TemplateArgument::Null:
  case TemplateArgument::Integral:
  case TemplateArgument::Declaration:
  case TemplateArgument::NullPtr:
  case TemplateArgument::Pack:
    // FIXME: Is this right?
    return TemplateArgumentLocInfo();
  }
  llvm_unreachable("unexpected template argument loc");
}

TemplateArgumentLoc
ASTReader::ReadTemplateArgumentLoc(ModuleFile &F,
                                   const RecordData &Record, unsigned &Index) {
  TemplateArgument Arg = ReadTemplateArgument(F, Record, Index);

  if (Arg.getKind() == TemplateArgument::Expression) {
    if (Record[Index++]) // bool InfoHasSameExpr.
      return TemplateArgumentLoc(Arg, TemplateArgumentLocInfo(Arg.getAsExpr()));
  }
  return TemplateArgumentLoc(Arg, GetTemplateArgumentLocInfo(F, Arg.getKind(),
                                                             Record, Index));
}

Decl *ASTReader::GetExternalDecl(uint32_t ID) {
  return GetDecl(ID);
}

uint64_t ASTReader::readCXXBaseSpecifiers(ModuleFile &M, const RecordData &Record, 
                                          unsigned &Idx){
  if (Idx >= Record.size())
    return 0;
  
  unsigned LocalID = Record[Idx++];
  return getGlobalBitOffset(M, M.CXXBaseSpecifiersOffsets[LocalID - 1]);
}

CXXBaseSpecifier *ASTReader::GetExternalCXXBaseSpecifiers(uint64_t Offset) {
  RecordLocation Loc = getLocalBitOffset(Offset);
  llvm::BitstreamCursor &Cursor = Loc.F->DeclsCursor;
  SavedStreamPosition SavedPosition(Cursor);
  Cursor.JumpToBit(Loc.Offset);
  ReadingKindTracker ReadingKind(Read_Decl, *this);
  RecordData Record;
  unsigned Code = Cursor.ReadCode();
  unsigned RecCode = Cursor.ReadRecord(Code, Record);
  if (RecCode != DECL_CXX_BASE_SPECIFIERS) {
    Error("Malformed AST file: missing C++ base specifiers");
    return 0;
  }

  unsigned Idx = 0;
  unsigned NumBases = Record[Idx++];
  void *Mem = Context.Allocate(sizeof(CXXBaseSpecifier) * NumBases);
  CXXBaseSpecifier *Bases = new (Mem) CXXBaseSpecifier [NumBases];
  for (unsigned I = 0; I != NumBases; ++I)
    Bases[I] = ReadCXXBaseSpecifier(*Loc.F, Record, Idx);
  return Bases;
}

serialization::DeclID 
ASTReader::getGlobalDeclID(ModuleFile &F, LocalDeclID LocalID) const {
  if (LocalID < NUM_PREDEF_DECL_IDS)
    return LocalID;

  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = F.DeclRemap.find(LocalID - NUM_PREDEF_DECL_IDS);
  assert(I != F.DeclRemap.end() && "Invalid index into decl index remap");
  
  return LocalID + I->second;
}

bool ASTReader::isDeclIDFromModule(serialization::GlobalDeclID ID,
                                   ModuleFile &M) const {
  GlobalDeclMapType::const_iterator I = GlobalDeclMap.find(ID);
  assert(I != GlobalDeclMap.end() && "Corrupted global declaration map");
  return &M == I->second;
}

ModuleFile *ASTReader::getOwningModuleFile(Decl *D) {
  if (!D->isFromASTFile())
    return 0;
  GlobalDeclMapType::const_iterator I = GlobalDeclMap.find(D->getGlobalID());
  assert(I != GlobalDeclMap.end() && "Corrupted global declaration map");
  return I->second;
}

SourceLocation ASTReader::getSourceLocationForDeclID(GlobalDeclID ID) {
  if (ID < NUM_PREDEF_DECL_IDS)
    return SourceLocation();
  
  unsigned Index = ID - NUM_PREDEF_DECL_IDS;

  if (Index > DeclsLoaded.size()) {
    Error("declaration ID out-of-range for AST file");
    return SourceLocation();
  }
  
  if (Decl *D = DeclsLoaded[Index])
    return D->getLocation();

  unsigned RawLocation = 0;
  RecordLocation Rec = DeclCursorForID(ID, RawLocation);
  return ReadSourceLocation(*Rec.F, RawLocation);
}

Decl *ASTReader::GetDecl(DeclID ID) {
  if (ID < NUM_PREDEF_DECL_IDS) {    
    switch ((PredefinedDeclIDs)ID) {
    case PREDEF_DECL_NULL_ID:
      return 0;
        
    case PREDEF_DECL_TRANSLATION_UNIT_ID:
      return Context.getTranslationUnitDecl();
        
    case PREDEF_DECL_OBJC_ID_ID:
      return Context.getObjCIdDecl();

    case PREDEF_DECL_OBJC_SEL_ID:
      return Context.getObjCSelDecl();

    case PREDEF_DECL_OBJC_CLASS_ID:
      return Context.getObjCClassDecl();
        
    case PREDEF_DECL_OBJC_PROTOCOL_ID:
      return Context.getObjCProtocolDecl();
        
    case PREDEF_DECL_INT_128_ID:
      return Context.getInt128Decl();

    case PREDEF_DECL_UNSIGNED_INT_128_ID:
      return Context.getUInt128Decl();
        
    case PREDEF_DECL_OBJC_INSTANCETYPE_ID:
      return Context.getObjCInstanceTypeDecl();

    case PREDEF_DECL_BUILTIN_VA_LIST_ID:
      return Context.getBuiltinVaListDecl();
    }
  }
  
  unsigned Index = ID - NUM_PREDEF_DECL_IDS;

  if (Index >= DeclsLoaded.size()) {
    assert(0 && "declaration ID out-of-range for AST file");
    Error("declaration ID out-of-range for AST file");
    return 0;
  }
  
  if (!DeclsLoaded[Index]) {
    ReadDeclRecord(ID);
    if (DeserializationListener)
      DeserializationListener->DeclRead(ID, DeclsLoaded[Index]);
  }

  return DeclsLoaded[Index];
}

DeclID ASTReader::mapGlobalIDToModuleFileGlobalID(ModuleFile &M, 
                                                  DeclID GlobalID) {
  if (GlobalID < NUM_PREDEF_DECL_IDS)
    return GlobalID;
  
  GlobalDeclMapType::const_iterator I = GlobalDeclMap.find(GlobalID);
  assert(I != GlobalDeclMap.end() && "Corrupted global declaration map");
  ModuleFile *Owner = I->second;

  llvm::DenseMap<ModuleFile *, serialization::DeclID>::iterator Pos
    = M.GlobalToLocalDeclIDs.find(Owner);
  if (Pos == M.GlobalToLocalDeclIDs.end())
    return 0;
      
  return GlobalID - Owner->BaseDeclID + Pos->second;
}

serialization::DeclID ASTReader::ReadDeclID(ModuleFile &F, 
                                            const RecordData &Record,
                                            unsigned &Idx) {
  if (Idx >= Record.size()) {
    Error("Corrupted AST file");
    return 0;
  }
  
  return getGlobalDeclID(F, Record[Idx++]);
}

/// \brief Resolve the offset of a statement into a statement.
///
/// This operation will read a new statement from the external
/// source each time it is called, and is meant to be used via a
/// LazyOffsetPtr (which is used by Decls for the body of functions, etc).
Stmt *ASTReader::GetExternalDeclStmt(uint64_t Offset) {
  // Switch case IDs are per Decl.
  ClearSwitchCaseIDs();

  // Offset here is a global offset across the entire chain.
  RecordLocation Loc = getLocalBitOffset(Offset);
  Loc.F->DeclsCursor.JumpToBit(Loc.Offset);
  return ReadStmtFromStream(*Loc.F);
}

namespace {
  class FindExternalLexicalDeclsVisitor {
    ASTReader &Reader;
    const DeclContext *DC;
    bool (*isKindWeWant)(Decl::Kind);
    
    SmallVectorImpl<Decl*> &Decls;
    bool PredefsVisited[NUM_PREDEF_DECL_IDS];

  public:
    FindExternalLexicalDeclsVisitor(ASTReader &Reader, const DeclContext *DC,
                                    bool (*isKindWeWant)(Decl::Kind),
                                    SmallVectorImpl<Decl*> &Decls)
      : Reader(Reader), DC(DC), isKindWeWant(isKindWeWant), Decls(Decls) 
    {
      for (unsigned I = 0; I != NUM_PREDEF_DECL_IDS; ++I)
        PredefsVisited[I] = false;
    }

    static bool visit(ModuleFile &M, bool Preorder, void *UserData) {
      if (Preorder)
        return false;

      FindExternalLexicalDeclsVisitor *This
        = static_cast<FindExternalLexicalDeclsVisitor *>(UserData);

      ModuleFile::DeclContextInfosMap::iterator Info
        = M.DeclContextInfos.find(This->DC);
      if (Info == M.DeclContextInfos.end() || !Info->second.LexicalDecls)
        return false;

      // Load all of the declaration IDs
      for (const KindDeclIDPair *ID = Info->second.LexicalDecls,
                               *IDE = ID + Info->second.NumLexicalDecls; 
           ID != IDE; ++ID) {
        if (This->isKindWeWant && !This->isKindWeWant((Decl::Kind)ID->first))
          continue;

        // Don't add predefined declarations to the lexical context more
        // than once.
        if (ID->second < NUM_PREDEF_DECL_IDS) {
          if (This->PredefsVisited[ID->second])
            continue;

          This->PredefsVisited[ID->second] = true;
        }

        if (Decl *D = This->Reader.GetLocalDecl(M, ID->second)) {
          if (!This->DC->isDeclInLexicalTraversal(D))
            This->Decls.push_back(D);
        }
      }

      return false;
    }
  };
}

ExternalLoadResult ASTReader::FindExternalLexicalDecls(const DeclContext *DC,
                                         bool (*isKindWeWant)(Decl::Kind),
                                         SmallVectorImpl<Decl*> &Decls) {
  // There might be lexical decls in multiple modules, for the TU at
  // least. Walk all of the modules in the order they were loaded.
  FindExternalLexicalDeclsVisitor Visitor(*this, DC, isKindWeWant, Decls);
  ModuleMgr.visitDepthFirst(&FindExternalLexicalDeclsVisitor::visit, &Visitor);
  ++NumLexicalDeclContextsRead;
  return ELR_Success;
}

namespace {

class DeclIDComp {
  ASTReader &Reader;
  ModuleFile &Mod;

public:
  DeclIDComp(ASTReader &Reader, ModuleFile &M) : Reader(Reader), Mod(M) {}

  bool operator()(LocalDeclID L, LocalDeclID R) const {
    SourceLocation LHS = getLocation(L);
    SourceLocation RHS = getLocation(R);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  bool operator()(SourceLocation LHS, LocalDeclID R) const {
    SourceLocation RHS = getLocation(R);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  bool operator()(LocalDeclID L, SourceLocation RHS) const {
    SourceLocation LHS = getLocation(L);
    return Reader.getSourceManager().isBeforeInTranslationUnit(LHS, RHS);
  }

  SourceLocation getLocation(LocalDeclID ID) const {
    return Reader.getSourceManager().getFileLoc(
            Reader.getSourceLocationForDeclID(Reader.getGlobalDeclID(Mod, ID)));
  }
};

}

void ASTReader::FindFileRegionDecls(FileID File,
                                    unsigned Offset, unsigned Length,
                                    SmallVectorImpl<Decl *> &Decls) {
  SourceManager &SM = getSourceManager();

  llvm::DenseMap<FileID, FileDeclsInfo>::iterator I = FileDeclIDs.find(File);
  if (I == FileDeclIDs.end())
    return;

  FileDeclsInfo &DInfo = I->second;
  if (DInfo.Decls.empty())
    return;

  SourceLocation
    BeginLoc = SM.getLocForStartOfFile(File).getLocWithOffset(Offset);
  SourceLocation EndLoc = BeginLoc.getLocWithOffset(Length);

  DeclIDComp DIDComp(*this, *DInfo.Mod);
  ArrayRef<serialization::LocalDeclID>::iterator
    BeginIt = std::lower_bound(DInfo.Decls.begin(), DInfo.Decls.end(),
                               BeginLoc, DIDComp);
  if (BeginIt != DInfo.Decls.begin())
    --BeginIt;

  // If we are pointing at a top-level decl inside an objc container, we need
  // to backtrack until we find it otherwise we will fail to report that the
  // region overlaps with an objc container.
  while (BeginIt != DInfo.Decls.begin() &&
         GetDecl(getGlobalDeclID(*DInfo.Mod, *BeginIt))
             ->isTopLevelDeclInObjCContainer())
    --BeginIt;

  ArrayRef<serialization::LocalDeclID>::iterator
    EndIt = std::upper_bound(DInfo.Decls.begin(), DInfo.Decls.end(),
                             EndLoc, DIDComp);
  if (EndIt != DInfo.Decls.end())
    ++EndIt;
  
  for (ArrayRef<serialization::LocalDeclID>::iterator
         DIt = BeginIt; DIt != EndIt; ++DIt)
    Decls.push_back(GetDecl(getGlobalDeclID(*DInfo.Mod, *DIt)));
}

namespace {
  /// \brief ModuleFile visitor used to perform name lookup into a
  /// declaration context.
  class DeclContextNameLookupVisitor {
    ASTReader &Reader;
    llvm::SmallVectorImpl<const DeclContext *> &Contexts;
    DeclarationName Name;
    SmallVectorImpl<NamedDecl *> &Decls;

  public:
    DeclContextNameLookupVisitor(ASTReader &Reader, 
                                 SmallVectorImpl<const DeclContext *> &Contexts, 
                                 DeclarationName Name,
                                 SmallVectorImpl<NamedDecl *> &Decls)
      : Reader(Reader), Contexts(Contexts), Name(Name), Decls(Decls) { }

    static bool visit(ModuleFile &M, void *UserData) {
      DeclContextNameLookupVisitor *This
        = static_cast<DeclContextNameLookupVisitor *>(UserData);

      // Check whether we have any visible declaration information for
      // this context in this module.
      ModuleFile::DeclContextInfosMap::iterator Info;
      bool FoundInfo = false;
      for (unsigned I = 0, N = This->Contexts.size(); I != N; ++I) {
        Info = M.DeclContextInfos.find(This->Contexts[I]);
        if (Info != M.DeclContextInfos.end() && 
            Info->second.NameLookupTableData) {
          FoundInfo = true;
          break;
        }
      }

      if (!FoundInfo)
        return false;
      
      // Look for this name within this module.
      ASTDeclContextNameLookupTable *LookupTable =
        Info->second.NameLookupTableData;
      ASTDeclContextNameLookupTable::iterator Pos
        = LookupTable->find(This->Name);
      if (Pos == LookupTable->end())
        return false;

      bool FoundAnything = false;
      ASTDeclContextNameLookupTrait::data_type Data = *Pos;
      for (; Data.first != Data.second; ++Data.first) {
        NamedDecl *ND = This->Reader.GetLocalDeclAs<NamedDecl>(M, *Data.first);
        if (!ND)
          continue;

        if (ND->getDeclName() != This->Name) {
          // A name might be null because the decl's redeclarable part is
          // currently read before reading its name. The lookup is triggered by
          // building that decl (likely indirectly), and so it is later in the
          // sense of "already existing" and can be ignored here.
          continue;
        }
      
        // Record this declaration.
        FoundAnything = true;
        This->Decls.push_back(ND);
      }

      return FoundAnything;
    }
  };
}

DeclContext::lookup_result
ASTReader::FindExternalVisibleDeclsByName(const DeclContext *DC,
                                          DeclarationName Name) {
  assert(DC->hasExternalVisibleStorage() &&
         "DeclContext has no visible decls in storage");
  if (!Name)
    return DeclContext::lookup_result(DeclContext::lookup_iterator(0),
                                      DeclContext::lookup_iterator(0));

  SmallVector<NamedDecl *, 64> Decls;
  
  // Compute the declaration contexts we need to look into. Multiple such
  // declaration contexts occur when two declaration contexts from disjoint
  // modules get merged, e.g., when two namespaces with the same name are 
  // independently defined in separate modules.
  SmallVector<const DeclContext *, 2> Contexts;
  Contexts.push_back(DC);
  
  if (DC->isNamespace()) {
    MergedDeclsMap::iterator Merged
      = MergedDecls.find(const_cast<Decl *>(cast<Decl>(DC)));
    if (Merged != MergedDecls.end()) {
      for (unsigned I = 0, N = Merged->second.size(); I != N; ++I)
        Contexts.push_back(cast<DeclContext>(GetDecl(Merged->second[I])));
    }
  }
  
  DeclContextNameLookupVisitor Visitor(*this, Contexts, Name, Decls);
  ModuleMgr.visit(&DeclContextNameLookupVisitor::visit, &Visitor);
  ++NumVisibleDeclContextsRead;
  SetExternalVisibleDeclsForName(DC, Name, Decls);
  return const_cast<DeclContext*>(DC)->lookup(Name);
}

namespace {
  /// \brief ModuleFile visitor used to retrieve all visible names in a
  /// declaration context.
  class DeclContextAllNamesVisitor {
    ASTReader &Reader;
    llvm::SmallVectorImpl<const DeclContext *> &Contexts;
    llvm::DenseMap<DeclarationName, SmallVector<NamedDecl *, 8> > &Decls;

  public:
    DeclContextAllNamesVisitor(ASTReader &Reader,
                               SmallVectorImpl<const DeclContext *> &Contexts,
                               llvm::DenseMap<DeclarationName,
                                           SmallVector<NamedDecl *, 8> > &Decls)
      : Reader(Reader), Contexts(Contexts), Decls(Decls) { }

    static bool visit(ModuleFile &M, void *UserData) {
      DeclContextAllNamesVisitor *This
        = static_cast<DeclContextAllNamesVisitor *>(UserData);

      // Check whether we have any visible declaration information for
      // this context in this module.
      ModuleFile::DeclContextInfosMap::iterator Info;
      bool FoundInfo = false;
      for (unsigned I = 0, N = This->Contexts.size(); I != N; ++I) {
        Info = M.DeclContextInfos.find(This->Contexts[I]);
        if (Info != M.DeclContextInfos.end() &&
            Info->second.NameLookupTableData) {
          FoundInfo = true;
          break;
        }
      }

      if (!FoundInfo)
        return false;

      ASTDeclContextNameLookupTable *LookupTable =
        Info->second.NameLookupTableData;
      bool FoundAnything = false;
      for (ASTDeclContextNameLookupTable::data_iterator
	     I = LookupTable->data_begin(), E = LookupTable->data_end();
	   I != E; ++I) {
        ASTDeclContextNameLookupTrait::data_type Data = *I;
        for (; Data.first != Data.second; ++Data.first) {
          NamedDecl *ND = This->Reader.GetLocalDeclAs<NamedDecl>(M,
                                                                 *Data.first);
          if (!ND)
            continue;

          // Record this declaration.
          FoundAnything = true;
          This->Decls[ND->getDeclName()].push_back(ND);
        }
      }

      return FoundAnything;
    }
  };
}

void ASTReader::completeVisibleDeclsMap(const DeclContext *DC) {
  if (!DC->hasExternalVisibleStorage())
    return;
  llvm::DenseMap<DeclarationName, llvm::SmallVector<NamedDecl*, 8> > Decls;

  // Compute the declaration contexts we need to look into. Multiple such
  // declaration contexts occur when two declaration contexts from disjoint
  // modules get merged, e.g., when two namespaces with the same name are
  // independently defined in separate modules.
  SmallVector<const DeclContext *, 2> Contexts;
  Contexts.push_back(DC);

  if (DC->isNamespace()) {
    MergedDeclsMap::iterator Merged
      = MergedDecls.find(const_cast<Decl *>(cast<Decl>(DC)));
    if (Merged != MergedDecls.end()) {
      for (unsigned I = 0, N = Merged->second.size(); I != N; ++I)
        Contexts.push_back(cast<DeclContext>(GetDecl(Merged->second[I])));
    }
  }

  DeclContextAllNamesVisitor Visitor(*this, Contexts, Decls);
  ModuleMgr.visit(&DeclContextAllNamesVisitor::visit, &Visitor);
  ++NumVisibleDeclContextsRead;

  for (llvm::DenseMap<DeclarationName,
                      llvm::SmallVector<NamedDecl*, 8> >::iterator
         I = Decls.begin(), E = Decls.end(); I != E; ++I) {
    SetExternalVisibleDeclsForName(DC, I->first, I->second);
  }
  const_cast<DeclContext *>(DC)->setHasExternalVisibleStorage(false);
}

/// \brief Under non-PCH compilation the consumer receives the objc methods
/// before receiving the implementation, and codegen depends on this.
/// We simulate this by deserializing and passing to consumer the methods of the
/// implementation before passing the deserialized implementation decl.
static void PassObjCImplDeclToConsumer(ObjCImplDecl *ImplD,
                                       ASTConsumer *Consumer) {
  assert(ImplD && Consumer);

  for (ObjCImplDecl::method_iterator
         I = ImplD->meth_begin(), E = ImplD->meth_end(); I != E; ++I)
    Consumer->HandleInterestingDecl(DeclGroupRef(*I));

  Consumer->HandleInterestingDecl(DeclGroupRef(ImplD));
}

void ASTReader::PassInterestingDeclsToConsumer() {
  assert(Consumer);
  while (!InterestingDecls.empty()) {
    Decl *D = InterestingDecls.front();
    InterestingDecls.pop_front();

    PassInterestingDeclToConsumer(D);
  }
}

void ASTReader::PassInterestingDeclToConsumer(Decl *D) {
  if (ObjCImplDecl *ImplD = dyn_cast<ObjCImplDecl>(D))
    PassObjCImplDeclToConsumer(ImplD, Consumer);
  else
    Consumer->HandleInterestingDecl(DeclGroupRef(D));
}

void ASTReader::StartTranslationUnit(ASTConsumer *Consumer) {
  this->Consumer = Consumer;

  if (!Consumer)
    return;

  for (unsigned I = 0, N = ExternalDefinitions.size(); I != N; ++I) {
    // Force deserialization of this decl, which will cause it to be queued for
    // passing to the consumer.
    GetDecl(ExternalDefinitions[I]);
  }
  ExternalDefinitions.clear();

  PassInterestingDeclsToConsumer();
}

void ASTReader::PrintStats() {
  std::fprintf(stderr, "*** AST File Statistics:\n");

  unsigned NumTypesLoaded
    = TypesLoaded.size() - std::count(TypesLoaded.begin(), TypesLoaded.end(),
                                      QualType());
  unsigned NumDeclsLoaded
    = DeclsLoaded.size() - std::count(DeclsLoaded.begin(), DeclsLoaded.end(),
                                      (Decl *)0);
  unsigned NumIdentifiersLoaded
    = IdentifiersLoaded.size() - std::count(IdentifiersLoaded.begin(),
                                            IdentifiersLoaded.end(),
                                            (IdentifierInfo *)0);
  unsigned NumMacrosLoaded
    = MacrosLoaded.size() - std::count(MacrosLoaded.begin(),
                                       MacrosLoaded.end(),
                                       (MacroInfo *)0);
  unsigned NumSelectorsLoaded
    = SelectorsLoaded.size() - std::count(SelectorsLoaded.begin(),
                                          SelectorsLoaded.end(),
                                          Selector());

  if (unsigned TotalNumSLocEntries = getTotalNumSLocs())
    std::fprintf(stderr, "  %u/%u source location entries read (%f%%)\n",
                 NumSLocEntriesRead, TotalNumSLocEntries,
                 ((float)NumSLocEntriesRead/TotalNumSLocEntries * 100));
  if (!TypesLoaded.empty())
    std::fprintf(stderr, "  %u/%u types read (%f%%)\n",
                 NumTypesLoaded, (unsigned)TypesLoaded.size(),
                 ((float)NumTypesLoaded/TypesLoaded.size() * 100));
  if (!DeclsLoaded.empty())
    std::fprintf(stderr, "  %u/%u declarations read (%f%%)\n",
                 NumDeclsLoaded, (unsigned)DeclsLoaded.size(),
                 ((float)NumDeclsLoaded/DeclsLoaded.size() * 100));
  if (!IdentifiersLoaded.empty())
    std::fprintf(stderr, "  %u/%u identifiers read (%f%%)\n",
                 NumIdentifiersLoaded, (unsigned)IdentifiersLoaded.size(),
                 ((float)NumIdentifiersLoaded/IdentifiersLoaded.size() * 100));
  if (!MacrosLoaded.empty())
    std::fprintf(stderr, "  %u/%u macros read (%f%%)\n",
                 NumMacrosLoaded, (unsigned)MacrosLoaded.size(),
                 ((float)NumMacrosLoaded/MacrosLoaded.size() * 100));
  if (!SelectorsLoaded.empty())
    std::fprintf(stderr, "  %u/%u selectors read (%f%%)\n",
                 NumSelectorsLoaded, (unsigned)SelectorsLoaded.size(),
                 ((float)NumSelectorsLoaded/SelectorsLoaded.size() * 100));
  if (TotalNumStatements)
    std::fprintf(stderr, "  %u/%u statements read (%f%%)\n",
                 NumStatementsRead, TotalNumStatements,
                 ((float)NumStatementsRead/TotalNumStatements * 100));
  if (TotalNumMacros)
    std::fprintf(stderr, "  %u/%u macros read (%f%%)\n",
                 NumMacrosRead, TotalNumMacros,
                 ((float)NumMacrosRead/TotalNumMacros * 100));
  if (TotalLexicalDeclContexts)
    std::fprintf(stderr, "  %u/%u lexical declcontexts read (%f%%)\n",
                 NumLexicalDeclContextsRead, TotalLexicalDeclContexts,
                 ((float)NumLexicalDeclContextsRead/TotalLexicalDeclContexts
                  * 100));
  if (TotalVisibleDeclContexts)
    std::fprintf(stderr, "  %u/%u visible declcontexts read (%f%%)\n",
                 NumVisibleDeclContextsRead, TotalVisibleDeclContexts,
                 ((float)NumVisibleDeclContextsRead/TotalVisibleDeclContexts
                  * 100));
  if (TotalNumMethodPoolEntries) {
    std::fprintf(stderr, "  %u/%u method pool entries read (%f%%)\n",
                 NumMethodPoolEntriesRead, TotalNumMethodPoolEntries,
                 ((float)NumMethodPoolEntriesRead/TotalNumMethodPoolEntries
                  * 100));
    std::fprintf(stderr, "  %u method pool misses\n", NumMethodPoolMisses);
  }
  std::fprintf(stderr, "\n");
  dump();
  std::fprintf(stderr, "\n");
}

template<typename Key, typename ModuleFile, unsigned InitialCapacity>
static void 
dumpModuleIDMap(StringRef Name,
                const ContinuousRangeMap<Key, ModuleFile *, 
                                         InitialCapacity> &Map) {
  if (Map.begin() == Map.end())
    return;
  
  typedef ContinuousRangeMap<Key, ModuleFile *, InitialCapacity> MapType;
  llvm::errs() << Name << ":\n";
  for (typename MapType::const_iterator I = Map.begin(), IEnd = Map.end(); 
       I != IEnd; ++I) {
    llvm::errs() << "  " << I->first << " -> " << I->second->FileName
      << "\n";
  }
}

void ASTReader::dump() {
  llvm::errs() << "*** PCH/ModuleFile Remappings:\n";
  dumpModuleIDMap("Global bit offset map", GlobalBitOffsetsMap);
  dumpModuleIDMap("Global source location entry map", GlobalSLocEntryMap);
  dumpModuleIDMap("Global type map", GlobalTypeMap);
  dumpModuleIDMap("Global declaration map", GlobalDeclMap);
  dumpModuleIDMap("Global identifier map", GlobalIdentifierMap);
  dumpModuleIDMap("Global macro map", GlobalMacroMap);
  dumpModuleIDMap("Global submodule map", GlobalSubmoduleMap);
  dumpModuleIDMap("Global selector map", GlobalSelectorMap);
  dumpModuleIDMap("Global preprocessed entity map", 
                  GlobalPreprocessedEntityMap);
  
  llvm::errs() << "\n*** PCH/Modules Loaded:";
  for (ModuleManager::ModuleConstIterator M = ModuleMgr.begin(), 
                                       MEnd = ModuleMgr.end();
       M != MEnd; ++M)
    (*M)->dump();
}

/// Return the amount of memory used by memory buffers, breaking down
/// by heap-backed versus mmap'ed memory.
void ASTReader::getMemoryBufferSizes(MemoryBufferSizes &sizes) const {
  for (ModuleConstIterator I = ModuleMgr.begin(),
      E = ModuleMgr.end(); I != E; ++I) {
    if (llvm::MemoryBuffer *buf = (*I)->Buffer.get()) {
      size_t bytes = buf->getBufferSize();
      switch (buf->getBufferKind()) {
        case llvm::MemoryBuffer::MemoryBuffer_Malloc:
          sizes.malloc_bytes += bytes;
          break;
        case llvm::MemoryBuffer::MemoryBuffer_MMap:
          sizes.mmap_bytes += bytes;
          break;
      }
    }
  }
}

void ASTReader::InitializeSema(Sema &S) {
  SemaObj = &S;
  S.addExternalSource(this);

  // Makes sure any declarations that were deserialized "too early"
  // still get added to the identifier's declaration chains.
  for (unsigned I = 0, N = PreloadedDecls.size(); I != N; ++I) {
    SemaObj->pushExternalDeclIntoScope(PreloadedDecls[I], 
                                       PreloadedDecls[I]->getDeclName());
  }
  PreloadedDecls.clear();

  // Load the offsets of the declarations that Sema references.
  // They will be lazily deserialized when needed.
  if (!SemaDeclRefs.empty()) {
    assert(SemaDeclRefs.size() == 2 && "More decl refs than expected!");
    if (!SemaObj->StdNamespace)
      SemaObj->StdNamespace = SemaDeclRefs[0];
    if (!SemaObj->StdBadAlloc)
      SemaObj->StdBadAlloc = SemaDeclRefs[1];
  }

  if (!FPPragmaOptions.empty()) {
    assert(FPPragmaOptions.size() == 1 && "Wrong number of FP_PRAGMA_OPTIONS");
    SemaObj->FPFeatures.fp_contract = FPPragmaOptions[0];
  }

  if (!OpenCLExtensions.empty()) {
    unsigned I = 0;
#define OPENCLEXT(nm)  SemaObj->OpenCLFeatures.nm = OpenCLExtensions[I++];
#include "clang/Basic/OpenCLExtensions.def"

    assert(OpenCLExtensions.size() == I && "Wrong number of OPENCL_EXTENSIONS");
  }
}

IdentifierInfo* ASTReader::get(const char *NameStart, const char *NameEnd) {
  // Note that we are loading an identifier.
  Deserializing AnIdentifier(this);
  
  IdentifierLookupVisitor Visitor(StringRef(NameStart, NameEnd - NameStart),
                                  /*PriorGeneration=*/0);
  ModuleMgr.visit(IdentifierLookupVisitor::visit, &Visitor);
  IdentifierInfo *II = Visitor.getIdentifierInfo();
  markIdentifierUpToDate(II);
  return II;
}

namespace clang {
  /// \brief An identifier-lookup iterator that enumerates all of the
  /// identifiers stored within a set of AST files.
  class ASTIdentifierIterator : public IdentifierIterator {
    /// \brief The AST reader whose identifiers are being enumerated.
    const ASTReader &Reader;

    /// \brief The current index into the chain of AST files stored in
    /// the AST reader.
    unsigned Index;

    /// \brief The current position within the identifier lookup table
    /// of the current AST file.
    ASTIdentifierLookupTable::key_iterator Current;

    /// \brief The end position within the identifier lookup table of
    /// the current AST file.
    ASTIdentifierLookupTable::key_iterator End;

  public:
    explicit ASTIdentifierIterator(const ASTReader &Reader);

    virtual StringRef Next();
  };
}

ASTIdentifierIterator::ASTIdentifierIterator(const ASTReader &Reader)
  : Reader(Reader), Index(Reader.ModuleMgr.size() - 1) {
  ASTIdentifierLookupTable *IdTable
    = (ASTIdentifierLookupTable *)Reader.ModuleMgr[Index].IdentifierLookupTable;
  Current = IdTable->key_begin();
  End = IdTable->key_end();
}

StringRef ASTIdentifierIterator::Next() {
  while (Current == End) {
    // If we have exhausted all of our AST files, we're done.
    if (Index == 0)
      return StringRef();

    --Index;
    ASTIdentifierLookupTable *IdTable
      = (ASTIdentifierLookupTable *)Reader.ModuleMgr[Index].
        IdentifierLookupTable;
    Current = IdTable->key_begin();
    End = IdTable->key_end();
  }

  // We have any identifiers remaining in the current AST file; return
  // the next one.
  std::pair<const char*, unsigned> Key = *Current;
  ++Current;
  return StringRef(Key.first, Key.second);
}

IdentifierIterator *ASTReader::getIdentifiers() const {
  return new ASTIdentifierIterator(*this);
}

namespace clang { namespace serialization {
  class ReadMethodPoolVisitor {
    ASTReader &Reader;
    Selector Sel;
    unsigned PriorGeneration;
    llvm::SmallVector<ObjCMethodDecl *, 4> InstanceMethods;
    llvm::SmallVector<ObjCMethodDecl *, 4> FactoryMethods;

  public:
    ReadMethodPoolVisitor(ASTReader &Reader, Selector Sel, 
                          unsigned PriorGeneration)
      : Reader(Reader), Sel(Sel), PriorGeneration(PriorGeneration) { }
    
    static bool visit(ModuleFile &M, void *UserData) {
      ReadMethodPoolVisitor *This
        = static_cast<ReadMethodPoolVisitor *>(UserData);
      
      if (!M.SelectorLookupTable)
        return false;
      
      // If we've already searched this module file, skip it now.
      if (M.Generation <= This->PriorGeneration)
        return true;

      ASTSelectorLookupTable *PoolTable
        = (ASTSelectorLookupTable*)M.SelectorLookupTable;
      ASTSelectorLookupTable::iterator Pos = PoolTable->find(This->Sel);
      if (Pos == PoolTable->end())
        return false;
      
      ++This->Reader.NumSelectorsRead;
      // FIXME: Not quite happy with the statistics here. We probably should
      // disable this tracking when called via LoadSelector.
      // Also, should entries without methods count as misses?
      ++This->Reader.NumMethodPoolEntriesRead;
      ASTSelectorLookupTrait::data_type Data = *Pos;
      if (This->Reader.DeserializationListener)
        This->Reader.DeserializationListener->SelectorRead(Data.ID, 
                                                           This->Sel);
      
      This->InstanceMethods.append(Data.Instance.begin(), Data.Instance.end());
      This->FactoryMethods.append(Data.Factory.begin(), Data.Factory.end());
      return true;
    }
    
    /// \brief Retrieve the instance methods found by this visitor.
    ArrayRef<ObjCMethodDecl *> getInstanceMethods() const { 
      return InstanceMethods; 
    }

    /// \brief Retrieve the instance methods found by this visitor.
    ArrayRef<ObjCMethodDecl *> getFactoryMethods() const { 
      return FactoryMethods;
    }
  };
} } // end namespace clang::serialization

/// \brief Add the given set of methods to the method list.
static void addMethodsToPool(Sema &S, ArrayRef<ObjCMethodDecl *> Methods,
                             ObjCMethodList &List) {
  for (unsigned I = 0, N = Methods.size(); I != N; ++I) {
    S.addMethodToGlobalList(&List, Methods[I]);
  }
}
                             
void ASTReader::ReadMethodPool(Selector Sel) {
  // Get the selector generation and update it to the current generation.
  unsigned &Generation = SelectorGeneration[Sel];
  unsigned PriorGeneration = Generation;
  Generation = CurrentGeneration;
  
  // Search for methods defined with this selector.
  ReadMethodPoolVisitor Visitor(*this, Sel, PriorGeneration);
  ModuleMgr.visit(&ReadMethodPoolVisitor::visit, &Visitor);
  
  if (Visitor.getInstanceMethods().empty() &&
      Visitor.getFactoryMethods().empty()) {
    ++NumMethodPoolMisses;
    return;
  }
  
  if (!getSema())
    return;
  
  Sema &S = *getSema();
  Sema::GlobalMethodPool::iterator Pos
    = S.MethodPool.insert(std::make_pair(Sel, Sema::GlobalMethods())).first;
  
  addMethodsToPool(S, Visitor.getInstanceMethods(), Pos->second.first);
  addMethodsToPool(S, Visitor.getFactoryMethods(), Pos->second.second);
}

void ASTReader::ReadKnownNamespaces(
                          SmallVectorImpl<NamespaceDecl *> &Namespaces) {
  Namespaces.clear();
  
  for (unsigned I = 0, N = KnownNamespaces.size(); I != N; ++I) {
    if (NamespaceDecl *Namespace 
                = dyn_cast_or_null<NamespaceDecl>(GetDecl(KnownNamespaces[I])))
      Namespaces.push_back(Namespace);
  }
}

void ASTReader::ReadTentativeDefinitions(
                  SmallVectorImpl<VarDecl *> &TentativeDefs) {
  for (unsigned I = 0, N = TentativeDefinitions.size(); I != N; ++I) {
    VarDecl *Var = dyn_cast_or_null<VarDecl>(GetDecl(TentativeDefinitions[I]));
    if (Var)
      TentativeDefs.push_back(Var);
  }
  TentativeDefinitions.clear();
}

void ASTReader::ReadUnusedFileScopedDecls(
                               SmallVectorImpl<const DeclaratorDecl *> &Decls) {
  for (unsigned I = 0, N = UnusedFileScopedDecls.size(); I != N; ++I) {
    DeclaratorDecl *D
      = dyn_cast_or_null<DeclaratorDecl>(GetDecl(UnusedFileScopedDecls[I]));
    if (D)
      Decls.push_back(D);
  }
  UnusedFileScopedDecls.clear();
}

void ASTReader::ReadDelegatingConstructors(
                                 SmallVectorImpl<CXXConstructorDecl *> &Decls) {
  for (unsigned I = 0, N = DelegatingCtorDecls.size(); I != N; ++I) {
    CXXConstructorDecl *D
      = dyn_cast_or_null<CXXConstructorDecl>(GetDecl(DelegatingCtorDecls[I]));
    if (D)
      Decls.push_back(D);
  }
  DelegatingCtorDecls.clear();
}

void ASTReader::ReadExtVectorDecls(SmallVectorImpl<TypedefNameDecl *> &Decls) {
  for (unsigned I = 0, N = ExtVectorDecls.size(); I != N; ++I) {
    TypedefNameDecl *D
      = dyn_cast_or_null<TypedefNameDecl>(GetDecl(ExtVectorDecls[I]));
    if (D)
      Decls.push_back(D);
  }
  ExtVectorDecls.clear();
}

void ASTReader::ReadDynamicClasses(SmallVectorImpl<CXXRecordDecl *> &Decls) {
  for (unsigned I = 0, N = DynamicClasses.size(); I != N; ++I) {
    CXXRecordDecl *D
      = dyn_cast_or_null<CXXRecordDecl>(GetDecl(DynamicClasses[I]));
    if (D)
      Decls.push_back(D);
  }
  DynamicClasses.clear();
}

void 
ASTReader::ReadLocallyScopedExternalDecls(SmallVectorImpl<NamedDecl *> &Decls) {
  for (unsigned I = 0, N = LocallyScopedExternalDecls.size(); I != N; ++I) {
    NamedDecl *D 
      = dyn_cast_or_null<NamedDecl>(GetDecl(LocallyScopedExternalDecls[I]));
    if (D)
      Decls.push_back(D);
  }
  LocallyScopedExternalDecls.clear();
}

void ASTReader::ReadReferencedSelectors(
       SmallVectorImpl<std::pair<Selector, SourceLocation> > &Sels) {
  if (ReferencedSelectorsData.empty())
    return;
  
  // If there are @selector references added them to its pool. This is for
  // implementation of -Wselector.
  unsigned int DataSize = ReferencedSelectorsData.size()-1;
  unsigned I = 0;
  while (I < DataSize) {
    Selector Sel = DecodeSelector(ReferencedSelectorsData[I++]);
    SourceLocation SelLoc
      = SourceLocation::getFromRawEncoding(ReferencedSelectorsData[I++]);
    Sels.push_back(std::make_pair(Sel, SelLoc));
  }
  ReferencedSelectorsData.clear();
}

void ASTReader::ReadWeakUndeclaredIdentifiers(
       SmallVectorImpl<std::pair<IdentifierInfo *, WeakInfo> > &WeakIDs) {
  if (WeakUndeclaredIdentifiers.empty())
    return;

  for (unsigned I = 0, N = WeakUndeclaredIdentifiers.size(); I < N; /*none*/) {
    IdentifierInfo *WeakId 
      = DecodeIdentifierInfo(WeakUndeclaredIdentifiers[I++]);
    IdentifierInfo *AliasId 
      = DecodeIdentifierInfo(WeakUndeclaredIdentifiers[I++]);
    SourceLocation Loc
      = SourceLocation::getFromRawEncoding(WeakUndeclaredIdentifiers[I++]);
    bool Used = WeakUndeclaredIdentifiers[I++];
    WeakInfo WI(AliasId, Loc);
    WI.setUsed(Used);
    WeakIDs.push_back(std::make_pair(WeakId, WI));
  }
  WeakUndeclaredIdentifiers.clear();
}

void ASTReader::ReadUsedVTables(SmallVectorImpl<ExternalVTableUse> &VTables) {
  for (unsigned Idx = 0, N = VTableUses.size(); Idx < N; /* In loop */) {
    ExternalVTableUse VT;
    VT.Record = dyn_cast_or_null<CXXRecordDecl>(GetDecl(VTableUses[Idx++]));
    VT.Location = SourceLocation::getFromRawEncoding(VTableUses[Idx++]);
    VT.DefinitionRequired = VTableUses[Idx++];
    VTables.push_back(VT);
  }
  
  VTableUses.clear();
}

void ASTReader::ReadPendingInstantiations(
       SmallVectorImpl<std::pair<ValueDecl *, SourceLocation> > &Pending) {
  for (unsigned Idx = 0, N = PendingInstantiations.size(); Idx < N;) {
    ValueDecl *D = cast<ValueDecl>(GetDecl(PendingInstantiations[Idx++]));
    SourceLocation Loc
      = SourceLocation::getFromRawEncoding(PendingInstantiations[Idx++]);

    Pending.push_back(std::make_pair(D, Loc));
  }  
  PendingInstantiations.clear();
}

void ASTReader::LoadSelector(Selector Sel) {
  // It would be complicated to avoid reading the methods anyway. So don't.
  ReadMethodPool(Sel);
}

void ASTReader::SetIdentifierInfo(IdentifierID ID, IdentifierInfo *II) {
  assert(ID && "Non-zero identifier ID required");
  assert(ID <= IdentifiersLoaded.size() && "identifier ID out of range");
  IdentifiersLoaded[ID - 1] = II;
  if (DeserializationListener)
    DeserializationListener->IdentifierRead(ID, II);
}

/// \brief Set the globally-visible declarations associated with the given
/// identifier.
///
/// If the AST reader is currently in a state where the given declaration IDs
/// cannot safely be resolved, they are queued until it is safe to resolve
/// them.
///
/// \param II an IdentifierInfo that refers to one or more globally-visible
/// declarations.
///
/// \param DeclIDs the set of declaration IDs with the name @p II that are
/// visible at global scope.
///
/// \param Nonrecursive should be true to indicate that the caller knows that
/// this call is non-recursive, and therefore the globally-visible declarations
/// will not be placed onto the pending queue.
void
ASTReader::SetGloballyVisibleDecls(IdentifierInfo *II,
                              const SmallVectorImpl<uint32_t> &DeclIDs,
                                   bool Nonrecursive) {
  if (NumCurrentElementsDeserializing && !Nonrecursive) {
    PendingIdentifierInfos.push_back(PendingIdentifierInfo());
    PendingIdentifierInfo &PII = PendingIdentifierInfos.back();
    PII.II = II;
    PII.DeclIDs.append(DeclIDs.begin(), DeclIDs.end());
    return;
  }

  for (unsigned I = 0, N = DeclIDs.size(); I != N; ++I) {
    NamedDecl *D = cast<NamedDecl>(GetDecl(DeclIDs[I]));
    if (SemaObj) {
      // Introduce this declaration into the translation-unit scope
      // and add it to the declaration chain for this identifier, so
      // that (unqualified) name lookup will find it.
      SemaObj->pushExternalDeclIntoScope(D, II);
    } else {
      // Queue this declaration so that it will be added to the
      // translation unit scope and identifier's declaration chain
      // once a Sema object is known.
      PreloadedDecls.push_back(D);
    }
  }
}

IdentifierInfo *ASTReader::DecodeIdentifierInfo(IdentifierID ID) {
  if (ID == 0)
    return 0;

  if (IdentifiersLoaded.empty()) {
    Error("no identifier table in AST file");
    return 0;
  }

  ID -= 1;
  if (!IdentifiersLoaded[ID]) {
    GlobalIdentifierMapType::iterator I = GlobalIdentifierMap.find(ID + 1);
    assert(I != GlobalIdentifierMap.end() && "Corrupted global identifier map");
    ModuleFile *M = I->second;
    unsigned Index = ID - M->BaseIdentifierID;
    const char *Str = M->IdentifierTableData + M->IdentifierOffsets[Index];

    // All of the strings in the AST file are preceded by a 16-bit length.
    // Extract that 16-bit length to avoid having to execute strlen().
    // NOTE: 'StrLenPtr' is an 'unsigned char*' so that we load bytes as
    //  unsigned integers.  This is important to avoid integer overflow when
    //  we cast them to 'unsigned'.
    const unsigned char *StrLenPtr = (const unsigned char*) Str - 2;
    unsigned StrLen = (((unsigned) StrLenPtr[0])
                       | (((unsigned) StrLenPtr[1]) << 8)) - 1;
    IdentifiersLoaded[ID]
      = &PP.getIdentifierTable().get(StringRef(Str, StrLen));
    if (DeserializationListener)
      DeserializationListener->IdentifierRead(ID + 1, IdentifiersLoaded[ID]);
  }

  return IdentifiersLoaded[ID];
}

IdentifierInfo *ASTReader::getLocalIdentifier(ModuleFile &M, unsigned LocalID) {
  return DecodeIdentifierInfo(getGlobalIdentifierID(M, LocalID));
}

IdentifierID ASTReader::getGlobalIdentifierID(ModuleFile &M, unsigned LocalID) {
  if (LocalID < NUM_PREDEF_IDENT_IDS)
    return LocalID;
  
  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = M.IdentifierRemap.find(LocalID - NUM_PREDEF_IDENT_IDS);
  assert(I != M.IdentifierRemap.end() 
         && "Invalid index into identifier index remap");
  
  return LocalID + I->second;
}

MacroInfo *ASTReader::getMacro(MacroID ID, MacroInfo *Hint) {
  if (ID == 0)
    return 0;

  if (MacrosLoaded.empty()) {
    Error("no macro table in AST file");
    return 0;
  }

  ID -= NUM_PREDEF_MACRO_IDS;
  if (!MacrosLoaded[ID]) {
    GlobalMacroMapType::iterator I
      = GlobalMacroMap.find(ID + NUM_PREDEF_MACRO_IDS);
    assert(I != GlobalMacroMap.end() && "Corrupted global macro map");
    ModuleFile *M = I->second;
    unsigned Index = ID - M->BaseMacroID;
    ReadMacroRecord(*M, M->MacroOffsets[Index], Hint);
  }

  return MacrosLoaded[ID];
}

MacroID ASTReader::getGlobalMacroID(ModuleFile &M, unsigned LocalID) {
  if (LocalID < NUM_PREDEF_MACRO_IDS)
    return LocalID;

  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = M.MacroRemap.find(LocalID - NUM_PREDEF_MACRO_IDS);
  assert(I != M.MacroRemap.end() && "Invalid index into macro index remap");

  return LocalID + I->second;
}

serialization::SubmoduleID
ASTReader::getGlobalSubmoduleID(ModuleFile &M, unsigned LocalID) {
  if (LocalID < NUM_PREDEF_SUBMODULE_IDS)
    return LocalID;
  
  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = M.SubmoduleRemap.find(LocalID - NUM_PREDEF_SUBMODULE_IDS);
  assert(I != M.SubmoduleRemap.end() 
         && "Invalid index into submodule index remap");
  
  return LocalID + I->second;
}

Module *ASTReader::getSubmodule(SubmoduleID GlobalID) {
  if (GlobalID < NUM_PREDEF_SUBMODULE_IDS) {
    assert(GlobalID == 0 && "Unhandled global submodule ID");
    return 0;
  }
  
  if (GlobalID > SubmodulesLoaded.size()) {
    Error("submodule ID out of range in AST file");
    return 0;
  }
  
  return SubmodulesLoaded[GlobalID - NUM_PREDEF_SUBMODULE_IDS];
}
                               
Selector ASTReader::getLocalSelector(ModuleFile &M, unsigned LocalID) {
  return DecodeSelector(getGlobalSelectorID(M, LocalID));
}

Selector ASTReader::DecodeSelector(serialization::SelectorID ID) {
  if (ID == 0)
    return Selector();

  if (ID > SelectorsLoaded.size()) {
    Error("selector ID out of range in AST file");
    return Selector();
  }

  if (SelectorsLoaded[ID - 1].getAsOpaquePtr() == 0) {
    // Load this selector from the selector table.
    GlobalSelectorMapType::iterator I = GlobalSelectorMap.find(ID);
    assert(I != GlobalSelectorMap.end() && "Corrupted global selector map");
    ModuleFile &M = *I->second;
    ASTSelectorLookupTrait Trait(*this, M);
    unsigned Idx = ID - M.BaseSelectorID - NUM_PREDEF_SELECTOR_IDS;
    SelectorsLoaded[ID - 1] =
      Trait.ReadKey(M.SelectorLookupTableData + M.SelectorOffsets[Idx], 0);
    if (DeserializationListener)
      DeserializationListener->SelectorRead(ID, SelectorsLoaded[ID - 1]);
  }

  return SelectorsLoaded[ID - 1];
}

Selector ASTReader::GetExternalSelector(serialization::SelectorID ID) {
  return DecodeSelector(ID);
}

uint32_t ASTReader::GetNumExternalSelectors() {
  // ID 0 (the null selector) is considered an external selector.
  return getTotalNumSelectors() + 1;
}

serialization::SelectorID
ASTReader::getGlobalSelectorID(ModuleFile &M, unsigned LocalID) const {
  if (LocalID < NUM_PREDEF_SELECTOR_IDS)
    return LocalID;
  
  ContinuousRangeMap<uint32_t, int, 2>::iterator I
    = M.SelectorRemap.find(LocalID - NUM_PREDEF_SELECTOR_IDS);
  assert(I != M.SelectorRemap.end() 
         && "Invalid index into selector index remap");
  
  return LocalID + I->second;
}

DeclarationName
ASTReader::ReadDeclarationName(ModuleFile &F, 
                               const RecordData &Record, unsigned &Idx) {
  DeclarationName::NameKind Kind = (DeclarationName::NameKind)Record[Idx++];
  switch (Kind) {
  case DeclarationName::Identifier:
    return DeclarationName(GetIdentifierInfo(F, Record, Idx));

  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    return DeclarationName(ReadSelector(F, Record, Idx));

  case DeclarationName::CXXConstructorName:
    return Context.DeclarationNames.getCXXConstructorName(
                          Context.getCanonicalType(readType(F, Record, Idx)));

  case DeclarationName::CXXDestructorName:
    return Context.DeclarationNames.getCXXDestructorName(
                          Context.getCanonicalType(readType(F, Record, Idx)));

  case DeclarationName::CXXConversionFunctionName:
    return Context.DeclarationNames.getCXXConversionFunctionName(
                          Context.getCanonicalType(readType(F, Record, Idx)));

  case DeclarationName::CXXOperatorName:
    return Context.DeclarationNames.getCXXOperatorName(
                                       (OverloadedOperatorKind)Record[Idx++]);

  case DeclarationName::CXXLiteralOperatorName:
    return Context.DeclarationNames.getCXXLiteralOperatorName(
                                       GetIdentifierInfo(F, Record, Idx));

  case DeclarationName::CXXUsingDirective:
    return DeclarationName::getUsingDirectiveName();
  }

  llvm_unreachable("Invalid NameKind!");
}

void ASTReader::ReadDeclarationNameLoc(ModuleFile &F,
                                       DeclarationNameLoc &DNLoc,
                                       DeclarationName Name,
                                      const RecordData &Record, unsigned &Idx) {
  switch (Name.getNameKind()) {
  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName:
    DNLoc.NamedType.TInfo = GetTypeSourceInfo(F, Record, Idx);
    break;

  case DeclarationName::CXXOperatorName:
    DNLoc.CXXOperatorName.BeginOpNameLoc
        = ReadSourceLocation(F, Record, Idx).getRawEncoding();
    DNLoc.CXXOperatorName.EndOpNameLoc
        = ReadSourceLocation(F, Record, Idx).getRawEncoding();
    break;

  case DeclarationName::CXXLiteralOperatorName:
    DNLoc.CXXLiteralOperatorName.OpNameLoc
        = ReadSourceLocation(F, Record, Idx).getRawEncoding();
    break;

  case DeclarationName::Identifier:
  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
  case DeclarationName::CXXUsingDirective:
    break;
  }
}

void ASTReader::ReadDeclarationNameInfo(ModuleFile &F,
                                        DeclarationNameInfo &NameInfo,
                                      const RecordData &Record, unsigned &Idx) {
  NameInfo.setName(ReadDeclarationName(F, Record, Idx));
  NameInfo.setLoc(ReadSourceLocation(F, Record, Idx));
  DeclarationNameLoc DNLoc;
  ReadDeclarationNameLoc(F, DNLoc, NameInfo.getName(), Record, Idx);
  NameInfo.setInfo(DNLoc);
}

void ASTReader::ReadQualifierInfo(ModuleFile &F, QualifierInfo &Info,
                                  const RecordData &Record, unsigned &Idx) {
  Info.QualifierLoc = ReadNestedNameSpecifierLoc(F, Record, Idx);
  unsigned NumTPLists = Record[Idx++];
  Info.NumTemplParamLists = NumTPLists;
  if (NumTPLists) {
    Info.TemplParamLists = new (Context) TemplateParameterList*[NumTPLists];
    for (unsigned i=0; i != NumTPLists; ++i)
      Info.TemplParamLists[i] = ReadTemplateParameterList(F, Record, Idx);
  }
}

TemplateName
ASTReader::ReadTemplateName(ModuleFile &F, const RecordData &Record, 
                            unsigned &Idx) {
  TemplateName::NameKind Kind = (TemplateName::NameKind)Record[Idx++];
  switch (Kind) {
  case TemplateName::Template:
      return TemplateName(ReadDeclAs<TemplateDecl>(F, Record, Idx));

  case TemplateName::OverloadedTemplate: {
    unsigned size = Record[Idx++];
    UnresolvedSet<8> Decls;
    while (size--)
      Decls.addDecl(ReadDeclAs<NamedDecl>(F, Record, Idx));

    return Context.getOverloadedTemplateName(Decls.begin(), Decls.end());
  }

  case TemplateName::QualifiedTemplate: {
    NestedNameSpecifier *NNS = ReadNestedNameSpecifier(F, Record, Idx);
    bool hasTemplKeyword = Record[Idx++];
    TemplateDecl *Template = ReadDeclAs<TemplateDecl>(F, Record, Idx);
    return Context.getQualifiedTemplateName(NNS, hasTemplKeyword, Template);
  }

  case TemplateName::DependentTemplate: {
    NestedNameSpecifier *NNS = ReadNestedNameSpecifier(F, Record, Idx);
    if (Record[Idx++])  // isIdentifier
      return Context.getDependentTemplateName(NNS,
                                               GetIdentifierInfo(F, Record, 
                                                                 Idx));
    return Context.getDependentTemplateName(NNS,
                                         (OverloadedOperatorKind)Record[Idx++]);
  }

  case TemplateName::SubstTemplateTemplateParm: {
    TemplateTemplateParmDecl *param
      = ReadDeclAs<TemplateTemplateParmDecl>(F, Record, Idx);
    if (!param) return TemplateName();
    TemplateName replacement = ReadTemplateName(F, Record, Idx);
    return Context.getSubstTemplateTemplateParm(param, replacement);
  }
      
  case TemplateName::SubstTemplateTemplateParmPack: {
    TemplateTemplateParmDecl *Param 
      = ReadDeclAs<TemplateTemplateParmDecl>(F, Record, Idx);
    if (!Param)
      return TemplateName();
    
    TemplateArgument ArgPack = ReadTemplateArgument(F, Record, Idx);
    if (ArgPack.getKind() != TemplateArgument::Pack)
      return TemplateName();
    
    return Context.getSubstTemplateTemplateParmPack(Param, ArgPack);
  }
  }

  llvm_unreachable("Unhandled template name kind!");
}

TemplateArgument
ASTReader::ReadTemplateArgument(ModuleFile &F,
                                const RecordData &Record, unsigned &Idx) {
  TemplateArgument::ArgKind Kind = (TemplateArgument::ArgKind)Record[Idx++];
  switch (Kind) {
  case TemplateArgument::Null:
    return TemplateArgument();
  case TemplateArgument::Type:
    return TemplateArgument(readType(F, Record, Idx));
  case TemplateArgument::Declaration: {
    ValueDecl *D = ReadDeclAs<ValueDecl>(F, Record, Idx);
    bool ForReferenceParam = Record[Idx++];
    return TemplateArgument(D, ForReferenceParam);
  }
  case TemplateArgument::NullPtr:
    return TemplateArgument(readType(F, Record, Idx), /*isNullPtr*/true);
  case TemplateArgument::Integral: {
    llvm::APSInt Value = ReadAPSInt(Record, Idx);
    QualType T = readType(F, Record, Idx);
    return TemplateArgument(Context, Value, T);
  }
  case TemplateArgument::Template: 
    return TemplateArgument(ReadTemplateName(F, Record, Idx));
  case TemplateArgument::TemplateExpansion: {
    TemplateName Name = ReadTemplateName(F, Record, Idx);
    llvm::Optional<unsigned> NumTemplateExpansions;
    if (unsigned NumExpansions = Record[Idx++])
      NumTemplateExpansions = NumExpansions - 1;
    return TemplateArgument(Name, NumTemplateExpansions);
  }
  case TemplateArgument::Expression:
    return TemplateArgument(ReadExpr(F));
  case TemplateArgument::Pack: {
    unsigned NumArgs = Record[Idx++];
    TemplateArgument *Args = new (Context) TemplateArgument[NumArgs];
    for (unsigned I = 0; I != NumArgs; ++I)
      Args[I] = ReadTemplateArgument(F, Record, Idx);
    return TemplateArgument(Args, NumArgs);
  }
  }

  llvm_unreachable("Unhandled template argument kind!");
}

TemplateParameterList *
ASTReader::ReadTemplateParameterList(ModuleFile &F,
                                     const RecordData &Record, unsigned &Idx) {
  SourceLocation TemplateLoc = ReadSourceLocation(F, Record, Idx);
  SourceLocation LAngleLoc = ReadSourceLocation(F, Record, Idx);
  SourceLocation RAngleLoc = ReadSourceLocation(F, Record, Idx);

  unsigned NumParams = Record[Idx++];
  SmallVector<NamedDecl *, 16> Params;
  Params.reserve(NumParams);
  while (NumParams--)
    Params.push_back(ReadDeclAs<NamedDecl>(F, Record, Idx));

  TemplateParameterList* TemplateParams =
    TemplateParameterList::Create(Context, TemplateLoc, LAngleLoc,
                                  Params.data(), Params.size(), RAngleLoc);
  return TemplateParams;
}

void
ASTReader::
ReadTemplateArgumentList(SmallVector<TemplateArgument, 8> &TemplArgs,
                         ModuleFile &F, const RecordData &Record,
                         unsigned &Idx) {
  unsigned NumTemplateArgs = Record[Idx++];
  TemplArgs.reserve(NumTemplateArgs);
  while (NumTemplateArgs--)
    TemplArgs.push_back(ReadTemplateArgument(F, Record, Idx));
}

/// \brief Read a UnresolvedSet structure.
void ASTReader::ReadUnresolvedSet(ModuleFile &F, UnresolvedSetImpl &Set,
                                  const RecordData &Record, unsigned &Idx) {
  unsigned NumDecls = Record[Idx++];
  while (NumDecls--) {
    NamedDecl *D = ReadDeclAs<NamedDecl>(F, Record, Idx);
    AccessSpecifier AS = (AccessSpecifier)Record[Idx++];
    Set.addDecl(D, AS);
  }
}

CXXBaseSpecifier
ASTReader::ReadCXXBaseSpecifier(ModuleFile &F,
                                const RecordData &Record, unsigned &Idx) {
  bool isVirtual = static_cast<bool>(Record[Idx++]);
  bool isBaseOfClass = static_cast<bool>(Record[Idx++]);
  AccessSpecifier AS = static_cast<AccessSpecifier>(Record[Idx++]);
  bool inheritConstructors = static_cast<bool>(Record[Idx++]);
  TypeSourceInfo *TInfo = GetTypeSourceInfo(F, Record, Idx);
  SourceRange Range = ReadSourceRange(F, Record, Idx);
  SourceLocation EllipsisLoc = ReadSourceLocation(F, Record, Idx);
  CXXBaseSpecifier Result(Range, isVirtual, isBaseOfClass, AS, TInfo, 
                          EllipsisLoc);
  Result.setInheritConstructors(inheritConstructors);
  return Result;
}

std::pair<CXXCtorInitializer **, unsigned>
ASTReader::ReadCXXCtorInitializers(ModuleFile &F, const RecordData &Record,
                                   unsigned &Idx) {
  CXXCtorInitializer **CtorInitializers = 0;
  unsigned NumInitializers = Record[Idx++];
  if (NumInitializers) {
    CtorInitializers
        = new (Context) CXXCtorInitializer*[NumInitializers];
    for (unsigned i=0; i != NumInitializers; ++i) {
      TypeSourceInfo *TInfo = 0;
      bool IsBaseVirtual = false;
      FieldDecl *Member = 0;
      IndirectFieldDecl *IndirectMember = 0;

      CtorInitializerType Type = (CtorInitializerType)Record[Idx++];
      switch (Type) {
      case CTOR_INITIALIZER_BASE:
        TInfo = GetTypeSourceInfo(F, Record, Idx);
        IsBaseVirtual = Record[Idx++];
        break;
          
      case CTOR_INITIALIZER_DELEGATING:
        TInfo = GetTypeSourceInfo(F, Record, Idx);
        break;

       case CTOR_INITIALIZER_MEMBER:
        Member = ReadDeclAs<FieldDecl>(F, Record, Idx);
        break;

       case CTOR_INITIALIZER_INDIRECT_MEMBER:
        IndirectMember = ReadDeclAs<IndirectFieldDecl>(F, Record, Idx);
        break;
      }

      SourceLocation MemberOrEllipsisLoc = ReadSourceLocation(F, Record, Idx);
      Expr *Init = ReadExpr(F);
      SourceLocation LParenLoc = ReadSourceLocation(F, Record, Idx);
      SourceLocation RParenLoc = ReadSourceLocation(F, Record, Idx);
      bool IsWritten = Record[Idx++];
      unsigned SourceOrderOrNumArrayIndices;
      SmallVector<VarDecl *, 8> Indices;
      if (IsWritten) {
        SourceOrderOrNumArrayIndices = Record[Idx++];
      } else {
        SourceOrderOrNumArrayIndices = Record[Idx++];
        Indices.reserve(SourceOrderOrNumArrayIndices);
        for (unsigned i=0; i != SourceOrderOrNumArrayIndices; ++i)
          Indices.push_back(ReadDeclAs<VarDecl>(F, Record, Idx));
      }

      CXXCtorInitializer *BOMInit;
      if (Type == CTOR_INITIALIZER_BASE) {
        BOMInit = new (Context) CXXCtorInitializer(Context, TInfo, IsBaseVirtual,
                                             LParenLoc, Init, RParenLoc,
                                             MemberOrEllipsisLoc);
      } else if (Type == CTOR_INITIALIZER_DELEGATING) {
        BOMInit = new (Context) CXXCtorInitializer(Context, TInfo, LParenLoc,
                                                   Init, RParenLoc);
      } else if (IsWritten) {
        if (Member)
          BOMInit = new (Context) CXXCtorInitializer(Context, Member, MemberOrEllipsisLoc,
                                               LParenLoc, Init, RParenLoc);
        else 
          BOMInit = new (Context) CXXCtorInitializer(Context, IndirectMember,
                                               MemberOrEllipsisLoc, LParenLoc,
                                               Init, RParenLoc);
      } else {
        BOMInit = CXXCtorInitializer::Create(Context, Member, MemberOrEllipsisLoc,
                                             LParenLoc, Init, RParenLoc,
                                             Indices.data(), Indices.size());
      }

      if (IsWritten)
        BOMInit->setSourceOrder(SourceOrderOrNumArrayIndices);
      CtorInitializers[i] = BOMInit;
    }
  }

  return std::make_pair(CtorInitializers, NumInitializers);
}

NestedNameSpecifier *
ASTReader::ReadNestedNameSpecifier(ModuleFile &F,
                                   const RecordData &Record, unsigned &Idx) {
  unsigned N = Record[Idx++];
  NestedNameSpecifier *NNS = 0, *Prev = 0;
  for (unsigned I = 0; I != N; ++I) {
    NestedNameSpecifier::SpecifierKind Kind
      = (NestedNameSpecifier::SpecifierKind)Record[Idx++];
    switch (Kind) {
    case NestedNameSpecifier::Identifier: {
      IdentifierInfo *II = GetIdentifierInfo(F, Record, Idx);
      NNS = NestedNameSpecifier::Create(Context, Prev, II);
      break;
    }

    case NestedNameSpecifier::Namespace: {
      NamespaceDecl *NS = ReadDeclAs<NamespaceDecl>(F, Record, Idx);
      NNS = NestedNameSpecifier::Create(Context, Prev, NS);
      break;
    }

    case NestedNameSpecifier::NamespaceAlias: {
      NamespaceAliasDecl *Alias =ReadDeclAs<NamespaceAliasDecl>(F, Record, Idx);
      NNS = NestedNameSpecifier::Create(Context, Prev, Alias);
      break;
    }

    case NestedNameSpecifier::TypeSpec:
    case NestedNameSpecifier::TypeSpecWithTemplate: {
      const Type *T = readType(F, Record, Idx).getTypePtrOrNull();
      if (!T)
        return 0;
      
      bool Template = Record[Idx++];
      NNS = NestedNameSpecifier::Create(Context, Prev, Template, T);
      break;
    }

    case NestedNameSpecifier::Global: {
      NNS = NestedNameSpecifier::GlobalSpecifier(Context);
      // No associated value, and there can't be a prefix.
      break;
    }
    }
    Prev = NNS;
  }
  return NNS;
}

NestedNameSpecifierLoc
ASTReader::ReadNestedNameSpecifierLoc(ModuleFile &F, const RecordData &Record, 
                                      unsigned &Idx) {
  unsigned N = Record[Idx++];
  NestedNameSpecifierLocBuilder Builder;
  for (unsigned I = 0; I != N; ++I) {
    NestedNameSpecifier::SpecifierKind Kind
      = (NestedNameSpecifier::SpecifierKind)Record[Idx++];
    switch (Kind) {
    case NestedNameSpecifier::Identifier: {
      IdentifierInfo *II = GetIdentifierInfo(F, Record, Idx);      
      SourceRange Range = ReadSourceRange(F, Record, Idx);
      Builder.Extend(Context, II, Range.getBegin(), Range.getEnd());
      break;
    }

    case NestedNameSpecifier::Namespace: {
      NamespaceDecl *NS = ReadDeclAs<NamespaceDecl>(F, Record, Idx);
      SourceRange Range = ReadSourceRange(F, Record, Idx);
      Builder.Extend(Context, NS, Range.getBegin(), Range.getEnd());
      break;
    }

    case NestedNameSpecifier::NamespaceAlias: {
      NamespaceAliasDecl *Alias =ReadDeclAs<NamespaceAliasDecl>(F, Record, Idx);
      SourceRange Range = ReadSourceRange(F, Record, Idx);
      Builder.Extend(Context, Alias, Range.getBegin(), Range.getEnd());
      break;
    }

    case NestedNameSpecifier::TypeSpec:
    case NestedNameSpecifier::TypeSpecWithTemplate: {
      bool Template = Record[Idx++];
      TypeSourceInfo *T = GetTypeSourceInfo(F, Record, Idx);
      if (!T)
        return NestedNameSpecifierLoc();
      SourceLocation ColonColonLoc = ReadSourceLocation(F, Record, Idx);

      // FIXME: 'template' keyword location not saved anywhere, so we fake it.
      Builder.Extend(Context, 
                     Template? T->getTypeLoc().getBeginLoc() : SourceLocation(),
                     T->getTypeLoc(), ColonColonLoc);
      break;
    }

    case NestedNameSpecifier::Global: {
      SourceLocation ColonColonLoc = ReadSourceLocation(F, Record, Idx);
      Builder.MakeGlobal(Context, ColonColonLoc);
      break;
    }
    }
  }
  
  return Builder.getWithLocInContext(Context);
}

SourceRange
ASTReader::ReadSourceRange(ModuleFile &F, const RecordData &Record,
                           unsigned &Idx) {
  SourceLocation beg = ReadSourceLocation(F, Record, Idx);
  SourceLocation end = ReadSourceLocation(F, Record, Idx);
  return SourceRange(beg, end);
}

/// \brief Read an integral value
llvm::APInt ASTReader::ReadAPInt(const RecordData &Record, unsigned &Idx) {
  unsigned BitWidth = Record[Idx++];
  unsigned NumWords = llvm::APInt::getNumWords(BitWidth);
  llvm::APInt Result(BitWidth, NumWords, &Record[Idx]);
  Idx += NumWords;
  return Result;
}

/// \brief Read a signed integral value
llvm::APSInt ASTReader::ReadAPSInt(const RecordData &Record, unsigned &Idx) {
  bool isUnsigned = Record[Idx++];
  return llvm::APSInt(ReadAPInt(Record, Idx), isUnsigned);
}

/// \brief Read a floating-point value
llvm::APFloat ASTReader::ReadAPFloat(const RecordData &Record, unsigned &Idx) {
  return llvm::APFloat(ReadAPInt(Record, Idx));
}

// \brief Read a string
std::string ASTReader::ReadString(const RecordData &Record, unsigned &Idx) {
  unsigned Len = Record[Idx++];
  std::string Result(Record.data() + Idx, Record.data() + Idx + Len);
  Idx += Len;
  return Result;
}

VersionTuple ASTReader::ReadVersionTuple(const RecordData &Record, 
                                         unsigned &Idx) {
  unsigned Major = Record[Idx++];
  unsigned Minor = Record[Idx++];
  unsigned Subminor = Record[Idx++];
  if (Minor == 0)
    return VersionTuple(Major);
  if (Subminor == 0)
    return VersionTuple(Major, Minor - 1);
  return VersionTuple(Major, Minor - 1, Subminor - 1);
}

CXXTemporary *ASTReader::ReadCXXTemporary(ModuleFile &F, 
                                          const RecordData &Record,
                                          unsigned &Idx) {
  CXXDestructorDecl *Decl = ReadDeclAs<CXXDestructorDecl>(F, Record, Idx);
  return CXXTemporary::Create(Context, Decl);
}

DiagnosticBuilder ASTReader::Diag(unsigned DiagID) {
  return Diag(SourceLocation(), DiagID);
}

DiagnosticBuilder ASTReader::Diag(SourceLocation Loc, unsigned DiagID) {
  return Diags.Report(Loc, DiagID);
}

/// \brief Retrieve the identifier table associated with the
/// preprocessor.
IdentifierTable &ASTReader::getIdentifierTable() {
  return PP.getIdentifierTable();
}

/// \brief Record that the given ID maps to the given switch-case
/// statement.
void ASTReader::RecordSwitchCaseID(SwitchCase *SC, unsigned ID) {
  assert((*CurrSwitchCaseStmts)[ID] == 0 &&
         "Already have a SwitchCase with this ID");
  (*CurrSwitchCaseStmts)[ID] = SC;
}

/// \brief Retrieve the switch-case statement with the given ID.
SwitchCase *ASTReader::getSwitchCaseWithID(unsigned ID) {
  assert((*CurrSwitchCaseStmts)[ID] != 0 && "No SwitchCase with this ID");
  return (*CurrSwitchCaseStmts)[ID];
}

void ASTReader::ClearSwitchCaseIDs() {
  CurrSwitchCaseStmts->clear();
}

void ASTReader::ReadComments() {
  std::vector<RawComment *> Comments;
  for (SmallVectorImpl<std::pair<llvm::BitstreamCursor,
                                 serialization::ModuleFile *> >::iterator
       I = CommentsCursors.begin(),
       E = CommentsCursors.end();
       I != E; ++I) {
    llvm::BitstreamCursor &Cursor = I->first;
    serialization::ModuleFile &F = *I->second;
    SavedStreamPosition SavedPosition(Cursor);

    RecordData Record;
    while (true) {
      unsigned Code = Cursor.ReadCode();
      if (Code == llvm::bitc::END_BLOCK)
        break;

      if (Code == llvm::bitc::ENTER_SUBBLOCK) {
        // No known subblocks, always skip them.
        Cursor.ReadSubBlockID();
        if (Cursor.SkipBlock()) {
          Error("malformed block record in AST file");
          return;
        }
        continue;
      }

      if (Code == llvm::bitc::DEFINE_ABBREV) {
        Cursor.ReadAbbrevRecord();
        continue;
      }

      // Read a record.
      Record.clear();
      switch ((CommentRecordTypes) Cursor.ReadRecord(Code, Record)) {
      case COMMENTS_RAW_COMMENT: {
        unsigned Idx = 0;
        SourceRange SR = ReadSourceRange(F, Record, Idx);
        RawComment::CommentKind Kind =
            (RawComment::CommentKind) Record[Idx++];
        bool IsTrailingComment = Record[Idx++];
        bool IsAlmostTrailingComment = Record[Idx++];
        Comments.push_back(new (Context) RawComment(SR, Kind,
                                                    IsTrailingComment,
                                                    IsAlmostTrailingComment));
        break;
      }
      }
    }
  }
  Context.Comments.addCommentsToFront(Comments);
}

void ASTReader::finishPendingActions() {
  while (!PendingIdentifierInfos.empty() || !PendingDeclChains.empty() ||
         !PendingMacroIDs.empty()) {
    // If any identifiers with corresponding top-level declarations have
    // been loaded, load those declarations now.
    while (!PendingIdentifierInfos.empty()) {
      SetGloballyVisibleDecls(PendingIdentifierInfos.front().II,
                              PendingIdentifierInfos.front().DeclIDs, true);
      PendingIdentifierInfos.pop_front();
    }
  
    // Load pending declaration chains.
    for (unsigned I = 0; I != PendingDeclChains.size(); ++I) {
      loadPendingDeclChain(PendingDeclChains[I]);
      PendingDeclChainsKnown.erase(PendingDeclChains[I]);
    }
    PendingDeclChains.clear();

    // Load any pending macro definitions.
    for (unsigned I = 0; I != PendingMacroIDs.size(); ++I) {
      // FIXME: std::move here
      SmallVector<MacroID, 2> GlobalIDs = PendingMacroIDs.begin()[I].second;
      MacroInfo *Hint = 0;
      for (unsigned IDIdx = 0, NumIDs = GlobalIDs.size(); IDIdx !=  NumIDs;
           ++IDIdx) {
        Hint = getMacro(GlobalIDs[IDIdx], Hint);
      }
    }
    PendingMacroIDs.clear();
  }
  
  // If we deserialized any C++ or Objective-C class definitions, any
  // Objective-C protocol definitions, or any redeclarable templates, make sure
  // that all redeclarations point to the definitions. Note that this can only 
  // happen now, after the redeclaration chains have been fully wired.
  for (llvm::SmallPtrSet<Decl *, 4>::iterator D = PendingDefinitions.begin(),
                                           DEnd = PendingDefinitions.end();
       D != DEnd; ++D) {
    if (TagDecl *TD = dyn_cast<TagDecl>(*D)) {
      if (const TagType *TagT = dyn_cast<TagType>(TD->TypeForDecl)) {
        // Make sure that the TagType points at the definition.
        const_cast<TagType*>(TagT)->decl = TD;
      }
      
      if (CXXRecordDecl *RD = dyn_cast<CXXRecordDecl>(*D)) {
        for (CXXRecordDecl::redecl_iterator R = RD->redecls_begin(),
                                         REnd = RD->redecls_end();
             R != REnd; ++R)
          cast<CXXRecordDecl>(*R)->DefinitionData = RD->DefinitionData;
        
      }

      continue;
    }
    
    if (ObjCInterfaceDecl *ID = dyn_cast<ObjCInterfaceDecl>(*D)) {
      // Make sure that the ObjCInterfaceType points at the definition.
      const_cast<ObjCInterfaceType *>(cast<ObjCInterfaceType>(ID->TypeForDecl))
        ->Decl = ID;
      
      for (ObjCInterfaceDecl::redecl_iterator R = ID->redecls_begin(),
                                           REnd = ID->redecls_end();
           R != REnd; ++R)
        R->Data = ID->Data;
      
      continue;
    }
    
    if (ObjCProtocolDecl *PD = dyn_cast<ObjCProtocolDecl>(*D)) {
      for (ObjCProtocolDecl::redecl_iterator R = PD->redecls_begin(),
                                          REnd = PD->redecls_end();
           R != REnd; ++R)
        R->Data = PD->Data;
      
      continue;
    }
    
    RedeclarableTemplateDecl *RTD
      = cast<RedeclarableTemplateDecl>(*D)->getCanonicalDecl();
    for (RedeclarableTemplateDecl::redecl_iterator R = RTD->redecls_begin(),
                                                REnd = RTD->redecls_end();
         R != REnd; ++R)
      R->Common = RTD->Common;
  }
  PendingDefinitions.clear();

  // Load the bodies of any functions or methods we've encountered. We do
  // this now (delayed) so that we can be sure that the declaration chains
  // have been fully wired up.
  for (PendingBodiesMap::iterator PB = PendingBodies.begin(),
                               PBEnd = PendingBodies.end();
       PB != PBEnd; ++PB) {
    if (FunctionDecl *FD = dyn_cast<FunctionDecl>(PB->first)) {
      // FIXME: Check for =delete/=default?
      // FIXME: Complain about ODR violations here?
      if (!getContext().getLangOpts().Modules || !FD->hasBody())
        FD->setLazyBody(PB->second);
      continue;
    }

    ObjCMethodDecl *MD = cast<ObjCMethodDecl>(PB->first);
    if (!getContext().getLangOpts().Modules || !MD->hasBody())
      MD->setLazyBody(PB->second);
  }
  PendingBodies.clear();
}

void ASTReader::FinishedDeserializing() {
  assert(NumCurrentElementsDeserializing &&
         "FinishedDeserializing not paired with StartedDeserializing");
  if (NumCurrentElementsDeserializing == 1) {
    // We decrease NumCurrentElementsDeserializing only after pending actions
    // are finished, to avoid recursively re-calling finishPendingActions().
    finishPendingActions();
  }
  --NumCurrentElementsDeserializing;

  if (NumCurrentElementsDeserializing == 0 &&
      Consumer && !PassingDeclsToConsumer) {
    // Guard variable to avoid recursively redoing the process of passing
    // decls to consumer.
    SaveAndRestore<bool> GuardPassingDeclsToConsumer(PassingDeclsToConsumer,
                                                     true);

    while (!InterestingDecls.empty()) {
      // We are not in recursive loading, so it's safe to pass the "interesting"
      // decls to the consumer.
      Decl *D = InterestingDecls.front();
      InterestingDecls.pop_front();
      PassInterestingDeclToConsumer(D);
    }
  }
}

ASTReader::ASTReader(Preprocessor &PP, ASTContext &Context,
                     StringRef isysroot, bool DisableValidation,
                     bool AllowASTWithCompilerErrors)
  : Listener(new PCHValidator(PP, *this)), DeserializationListener(0),
    SourceMgr(PP.getSourceManager()), FileMgr(PP.getFileManager()),
    Diags(PP.getDiagnostics()), SemaObj(0), PP(PP), Context(Context),
    Consumer(0), ModuleMgr(PP.getFileManager()),
    isysroot(isysroot), DisableValidation(DisableValidation),
    AllowASTWithCompilerErrors(AllowASTWithCompilerErrors), 
    CurrentGeneration(0), CurrSwitchCaseStmts(&SwitchCaseStmts),
    NumSLocEntriesRead(0), TotalNumSLocEntries(0), 
    NumStatementsRead(0), TotalNumStatements(0), NumMacrosRead(0), 
    TotalNumMacros(0), NumSelectorsRead(0), NumMethodPoolEntriesRead(0), 
    NumMethodPoolMisses(0), TotalNumMethodPoolEntries(0), 
    NumLexicalDeclContextsRead(0), TotalLexicalDeclContexts(0), 
    NumVisibleDeclContextsRead(0), TotalVisibleDeclContexts(0),
    TotalModulesSizeInBits(0), NumCurrentElementsDeserializing(0),
    PassingDeclsToConsumer(false),
    NumCXXBaseSpecifiersLoaded(0)
{
  SourceMgr.setExternalSLocEntrySource(this);
}

ASTReader::~ASTReader() {
  for (DeclContextVisibleUpdatesPending::iterator
           I = PendingVisibleUpdates.begin(),
           E = PendingVisibleUpdates.end();
       I != E; ++I) {
    for (DeclContextVisibleUpdates::iterator J = I->second.begin(),
                                             F = I->second.end();
         J != F; ++J)
      delete J->first;
  }
}
