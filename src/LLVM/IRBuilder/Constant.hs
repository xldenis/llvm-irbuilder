{-# LANGUAGE FlexibleContexts #-}

module LLVM.IRBuilder.Constant where
import Data.Word

import qualified LLVM.AST.Constant as C
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Monad
import LLVM.AST hiding (args, dests)

import LLVM.AST.Constant
import LLVM.AST.Float

int64 :: MonadIRBuilder m => Integer -> m Operand
int64 = pure . ConstantOperand . Int 64

double :: MonadIRBuilder m => Double -> m Operand
double = pure . ConstantOperand . Float . Double

single :: MonadIRBuilder m => Float -> m Operand
single = pure . ConstantOperand . Float . Single

half :: MonadIRBuilder m => Word16 -> m Operand
half = pure . ConstantOperand . Float . Half

