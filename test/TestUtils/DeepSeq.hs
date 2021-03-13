{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtils.DeepSeq () where

import Control.DeepSeq (NFData (..))

#if MIN_VERSION_template_haskell(2,16,0)
import Control.DeepSeq (rwhnf)
import GHC.ForeignPtr (ForeignPtr)
#endif
import Language.Haskell.TH.Syntax

instance NFData AnnTarget
instance NFData Bang
instance NFData Body
instance NFData Callconv
instance NFData Clause
instance NFData Con
instance NFData Dec
instance NFData DerivClause
instance NFData DerivStrategy
instance NFData Exp
instance NFData FamilyResultSig
instance NFData Fixity
instance NFData FixityDirection
instance NFData Foreign
instance NFData FunDep
instance NFData Guard
instance NFData InjectivityAnn
instance NFData Inline
instance NFData Lit
instance NFData Match
instance NFData ModName
instance NFData Name
instance NFData NameFlavour
instance NFData NameSpace
instance NFData OccName
instance NFData Overlap
instance NFData Pat
instance NFData PatSynArgs
instance NFData PatSynDir
instance NFData Phases
instance NFData PkgName
instance NFData Pragma
instance NFData Range
instance NFData Role
instance NFData RuleBndr
instance NFData RuleMatch
instance NFData Safety
instance NFData SourceStrictness
instance NFData SourceUnpackedness
instance NFData Stmt
instance NFData Type
instance NFData TypeFamilyHead
instance NFData TyLit
instance NFData TySynEqn
instance NFData TyVarBndr

#if MIN_VERSION_template_haskell(2,16,0)
instance NFData Bytes

instance NFData (ForeignPtr a) where
  rnf = rwhnf
#endif
