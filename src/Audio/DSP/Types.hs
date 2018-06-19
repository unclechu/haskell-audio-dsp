-- Author: Viacheslav Lotsmanov
-- License: GPLv3 (see LICENSE file)

{-# LANGUAGE UnicodeSyntax #-}

module Audio.DSP.Types where

import Foreign.C.Types (CFloat)


type F               = CFloat
type 𝔹               = Bool
type IsActive        = 𝔹
type IsMovingForward = 𝔹
type IsIncreasing    = 𝔹
type Milliseconds    = F
type SampleRate      = Int
type Decibels        = F
type Coefficient     = F
