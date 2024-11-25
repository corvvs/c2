module Exception(ExceptTT, throwError) where

import Control.Monad.Except
import qualified Data.Text as T

type ExceptTT a = ExceptT T.Text IO a
