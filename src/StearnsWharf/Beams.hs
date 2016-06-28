{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module StearnsWharf.Beams where

import Data.Maybe (fromJust)
import Data.List (find)

-- import Data.Packed.ST (STMatrix,modifyMatrix,STVector,modifyVector)

import Control.Monad.ST (ST)

import Numeric.LinearAlgebra ((<>),Matrix,Vector,fromList,fromLists,disp,dispf,tr)

import Numeric.LinearAlgebra.Devel (modifyVector,modifyMatrix,STMatrix,STVector)

import qualified StearnsWharf.Nodes as N
