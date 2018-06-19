-- Author: Viacheslav Lotsmanov
-- License: GPLv3 (see LICENSE file)

{-# LANGUAGE UnicodeSyntax #-}

module Audio.DSP.Utils where

import Prelude.Unicode
import Data.Proxy
import Data.Maybe
import Data.Monoid ((<>))
import Control.Monad.Exception.Synchronous (Exceptional (Success, Exception))
import System.Exit
import System.IO (hPutStrLn, stderr)
import qualified Foreign.C.Error as Foreign
import Sound.JACK.Exception (Errno (Errno))
import Audio.DSP.Types


(◇) = (<>); infixr 6 ◇; (◇) ∷ Monoid α ⇒ α → α → α;   {-# INLINE (◇) #-}
(×) = (*);  infixl 7 ×; (×) ∷ Num α ⇒ α → α → α;      {-# INLINE (×) #-}
(□) = (**); infixr 8 □; (□) ∷ Floating α ⇒ α → α → α; {-# INLINE (□) #-}

class Num α ⇒ Step α where step ∷ α

-- msToSamples ∷ SampleRate → Milliseconds → NFrames
-- msToSamples sr ms = NFrames $ round $ fromIntegral sr × ms ÷ 1000

dbToCoefficient ∷ Decibels → Coefficient
dbToCoefficient dB | dB > -90.0 = 10 □ (dB × 0.05)
                   | otherwise  = 0

safeSucc, safePred ∷ (Enum α, Bounded α, Ord α) ⇒ α → Maybe α
safeSucc x | x ≥ maxBound = Nothing | otherwise = Just $ succ x
safePred x | x ≤ minBound = Nothing | otherwise = Just $ pred x

boundedPlus, boundedMinus ∷ (Num α, Bounded α, Ord α) ⇒ α → α → α
boundedPlus  a b = let x = a + b in if x ≥ maxBound then maxBound else x
boundedMinus a b = let x = a - b in if x ≤ minBound then minBound else x

-- Get value from a..b range to c..d range.
-- For example: `rangeShift (10, 20) (100, 200) 15 = 150`
rangeShift ∷ (Num α, Fractional α) ⇒ (α, α) → (α, α) → α → α
rangeShift (a, b) (c, d) x = c + ((x - a) ÷ (b - a)) × (d - c)

fracNShow ∷ (RealFrac α, Floating α, Show α) ⇒ Int → α → String
fracNShow n x = if length remainder > n
                   then show (succ a) ◇ "." ◇ replicate n '0'
                   else show a ◇ "." ◇ remainderWithZeros

  where (a, b) = properFraction x
        remainder = show $ round $ b × (10 □ fromIntegral n)
        needZeros = n - length remainder
        remainderWithZeros = replicate needZeros '0' ◇ remainder

handleException
  ∷ Proxy ε
  → String
  → Maybe (String → ε → String)
  → Exceptional ε α
  → IO α
handleException Proxy _ _ (Success   x) = pure x
handleException Proxy m f (Exception e) = do
  hPutStrLn stderr $ fromMaybe m $ f <*> pure m <*> pure e
  exitWith $ ExitFailure 1

errnoExceptionReport ∷ String → Errno α → String
errnoExceptionReport m (Errno (Foreign.Errno x)) = m ◇ ": Errno " ◇ show x
errnoExceptionReport m _ = m
