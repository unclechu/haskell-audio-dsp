-- | “Thick Distortion” JACK standalone application.
-- Author: Viacheslav Lotsmanov
-- License: GPLv3 (see LICENSE file)

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Prelude.Unicode
import qualified Foreign.C.Error as Foreign
import System.Exit
import Sound.JACK as JACK hiding (Port)
import Sound.JACK.Audio
import Sound.JACK.Exception (Status, Errno (Errno), PortRegister)
import Graphics.Vty
import Control.Monad
import Control.Monad.Exception.Synchronous (runExceptionalT)
import Data.Function ((&))
import Data.Primitive.Ptr (Ptr, nullPtr)
import Data.Array.Storable (readArray, writeArray)
import Data.Maybe
import Data.IORef
import Data.Default
import Data.Proxy
import Audio.DSP.Types
import Audio.DSP.Utils


newtype InputGainKnob
      = InputGainKnob Decibels
        deriving (Num, Fractional, Real, RealFrac, Eq, Ord)

inputGainKnobPrecalc ∷ InputGainKnob → Coefficient
inputGainKnobPrecalc (InputGainKnob x) = dbToCoefficient x

instance Show    InputGainKnob where show (InputGainKnob x) = "Input Gain: "
                                                            ◇ show x
instance Default InputGainKnob where def      = InputGainKnob 0
instance Step    InputGainKnob where step     = InputGainKnob 0.5
instance Bounded InputGainKnob where minBound = InputGainKnob (-20)
                                     maxBound = InputGainKnob 80

newtype OutputGainKnob
      = OutputGainKnob Decibels
        deriving (Num, Fractional, Real, RealFrac, Eq, Ord)

outputGainKnobPrecalc ∷ OutputGainKnob → Coefficient
outputGainKnobPrecalc (OutputGainKnob x) = dbToCoefficient x

instance Show    OutputGainKnob where show (OutputGainKnob x) = "Output Gain: "
                                                              ◇ show x
instance Default OutputGainKnob where def      = OutputGainKnob 0
instance Step    OutputGainKnob where step     = OutputGainKnob 0.5
instance Bounded OutputGainKnob where minBound = OutputGainKnob (-90)
                                      maxBound = OutputGainKnob 12

newtype ThicknessKnob
      = ThicknessKnob F
        deriving (Num, Fractional, Real, RealFrac, Eq, Ord)

thicknessKnobPrecalc ∷ ThicknessKnob → (F, F)
thicknessKnobPrecalc (ThicknessKnob x) = (1 - x, x)

instance Show    ThicknessKnob where show (ThicknessKnob x) = "Thickness: "
                                                            ◇ fracNShow 2 x
instance Default ThicknessKnob where def      = ThicknessKnob 0.80
instance Step    ThicknessKnob where step     = ThicknessKnob 0.01
instance Bounded ThicknessKnob where minBound = ThicknessKnob 0.01
                                     maxBound = ThicknessKnob 1

-- Each knob have precalculated value in second part of tuple which is used in
-- real-time critical jack process callback to avoid wasting time there.
data Knobs
   = Knobs
   { inputGainKnob  ∷ (InputGainKnob,  Coefficient)
   , thicknessKnob  ∷ (ThicknessKnob,  (F, F))
   , outputGainKnob ∷ (OutputGainKnob, Coefficient)
   }

data Knob
   = InputGainKnob'
   | ThicknessKnob'
   | OutputGainKnob'
     deriving (Eq, Enum, Bounded, Ord, Show)

measureSfx ∷ Knob → String
measureSfx InputGainKnob'  = "dB"
measureSfx ThicknessKnob'  = ""
measureSfx OutputGainKnob' = "dB"


main ∷ IO ()
main = do
  selectedKnobRef ← newIORef (minBound ∷ Knob)
  (jackClient, sr) ← preInitJACK

  knobsRef ← newIORef Knobs { inputGainKnob  = (def, inputGainKnobPrecalc  def)
                            , thicknessKnob  = (def, thicknessKnobPrecalc  def)
                            , outputGainKnob = (def, outputGainKnobPrecalc def)
                            }

  jackClientDeactivate ← initJACK jackClient knobsRef
  ui sr knobsRef selectedKnobRef jackClientDeactivate


jackProcess
  ∷ IORef Knobs
  → Port JACK.Input
  → Port JACK.Output
  → NFrames
  → Ptr ()
  → IO Foreign.Errno
jackProcess knobsRef inPort outPort nframes@(NFrames _nframes) _ = do
  inArr  ← getBufferArray inPort  nframes
  outArr ← getBufferArray outPort nframes
  knobs  ← readIORef knobsRef

  !buf   ← -- We able to read last sample from previous buffer output array.
           readArray outArr lastNFrame

  let (_, inputGainCo)      = inputGainKnob  knobs
      (_, (co, coOpposite)) = thicknessKnob  knobs
      (_, outputGainCo)     = outputGainKnob knobs

      process lastSample = (× outputGainCo)
                         ∘ f
                         ∘ hardLimiter
                         ∘ (× inputGainCo)

        where f x = (lastSample × coOpposite) + (x × co)

  forM_ (nframesIndices nframes) $ \i@(NFrames _i) →
    let lastSample = if _i ≡ minBound
                        then pure buf
                        else readArray outArr (NFrames $ pred _i)
        currentSample = readArray inArr i
     in process <$> lastSample <*> currentSample >>= writeArray outArr i

  pure Foreign.eOK

  where hardLimiter = min 1 ∘ max (-1)
        lastNFrame  = NFrames $ pred _nframes


preInitJACK ∷ IO (Client, SampleRate)
preInitJACK = do
  !jackClient ←
    runExceptionalT (newClientDefault clientName) >>=
      handleException (Proxy ∷ Proxy (Status ()))
        "Failed to initialize JACK client"
        Nothing

  (jackClient,) <$> getSampleRate jackClient

-- Returns JACK client deactivator monad
initJACK ∷ Client → IORef Knobs → IO (IO ())
initJACK jackClient knobsRef = do
  ((inPort ∷ Port JACK.Input), (outPort ∷ Port JACK.Output)) ←
    handleException (Proxy ∷ Proxy (PortRegister ()))
      "Failed to register JACK ports" Nothing =<< runExceptionalT
        ((,) <$> newPort jackClient "in" <*> newPort jackClient "out")

  processPtr ← makeProcess $ jackProcess knobsRef inPort outPort

  runExceptionalT (setProcess jackClient processPtr nullPtr) >>=
    handleException (Proxy ∷ Proxy (Errno ()))
      "Failed to set JACK process callback"
      (Just errnoExceptionReport)

  runExceptionalT (activate jackClient) >>=
    handleException (Proxy ∷ Proxy (Errno ()))
      "Failed to activate JACK client"
      (Just errnoExceptionReport)

  pure $
    runExceptionalT (deactivate jackClient) >>=
      handleException (Proxy ∷ Proxy (Errno ()))
        "Failed to deactivate JACK client"
        (Just errnoExceptionReport)


ui ∷ SampleRate → IORef Knobs → IORef Knob → IO () → IO ()
ui sr knobsRef selectedKnobRef jackClientDeactivate = do
  cfg ← standardIOConfig
  vty ← mkVty cfg
  render vty

  forever $ do
    e ← nextEvent vty
    render vty

    case e of
         EvKey KEsc []             → done vty
         EvKey (KChar 'c') [MCtrl] → done vty

         EvKey KLeft []            → changeValue False >> render vty
         EvKey KRight []           → changeValue True  >> render vty

         -- Ctrl modifier makes it be done twice
         EvKey KLeft [MCtrl]       → changeValue False >> changeValue False
                                                       >> render vty
         EvKey KRight [MCtrl]      → changeValue True  >> changeValue True
                                                       >> render vty

         -- Shift modifier makes it be done four times
         EvKey KLeft [MShift]      → changeValue False >> changeValue False
                                                       >> changeValue False
                                                       >> changeValue False
                                                       >> render vty
         EvKey KRight [MShift]     → changeValue True  >> changeValue True
                                                       >> changeValue True
                                                       >> changeValue True
                                                       >> render vty

         EvKey KUp []              → select      False >> render vty
         EvKey KDown []            → select      True  >> render vty

         _                         → pure ()

  where
    knobs = [minBound..maxBound] ∷ [Knob]
    done vty = shutdown vty >> jackClientDeactivate >> exitWith ExitSuccess

    render vty = update vty =<<
      draw <$> readIORef knobsRef <*> readIORef selectedKnobRef

    draw ∷ Knobs → Knob → Picture
    draw knobs' selectedKnob = picForImage $ foldl reducer (f kx) kxs
      where (kx : kxs) = knobs
            reducer acc x = acc `vertJoin` f x

            f k@InputGainKnob'
              = hslider (measureSfx k) (k ≡ selectedKnob)
              $ fst $ inputGainKnob knobs'
            f k@ThicknessKnob'
              = hslider (measureSfx k) (k ≡ selectedKnob)
              $ fst $ thicknessKnob knobs'
            f k@OutputGainKnob'
              = hslider (measureSfx k) (k ≡ selectedKnob)
              $ fst $ outputGainKnob knobs'

    hslider ∷ (Bounded α, RealFrac α, Show α) ⇒ String → IsActive → α → Image
    hslider sfx isActive knob =
      string indicatorAttr filled
        `horizJoin` string defAttr " "
        `horizJoin` string valueAttr (show knob)
        `horizJoin` string valueAttr " "
        `horizJoin` string sfxAttr sfx

      where lengthLimit = 40

            relativeValue =
              rangeShift (minBound, maxBound) (0, lengthLimit) knob

            filled = replicate (round relativeValue) '◆'
                   ◇ replicate (round $ lengthLimit - relativeValue) ' '

            indicatorAttr = defAttr
              `withBackColor` (if isActive then cyan else black)
              `withForeColor` brightGreen

            sfxAttr = valueAttr `withStyle` bold

            valueAttr =
              defAttr `withForeColor`
                if isActive then brightCyan else brightGreen

    select ∷ IsMovingForward → IO ()
    select forward = selectedKnobRef `modifyIORef'` \x →
      fromMaybe x $ x & if forward then safeSucc else safePred

    changeValue ∷ IsIncreasing → IO ()
    changeValue increasing = do
      selectedKnob ← readIORef selectedKnobRef
      knobsRef `modifyIORef'` \x →
        case selectedKnob of
             InputGainKnob' →
               let v = fst (inputGainKnob x) `operation` step
                in x { inputGainKnob = (v, inputGainKnobPrecalc v) }
             ThicknessKnob' →
               let v = fst (thicknessKnob x) `operation` step
                in x { thicknessKnob = (v, thicknessKnobPrecalc v) }
             OutputGainKnob' →
               let v = fst (outputGainKnob x) `operation` step
                in x { outputGainKnob = (v, outputGainKnobPrecalc v) }

      where operation = if increasing then boundedPlus else boundedMinus

clientName ∷ String
clientName = "ThickDist"
