module Data.AS3.AST.ThirdParty where

import           Control.Monad
import           Data.AS3.AST.Def
import           Text.Parsec

{-
The following code is copied from the json package, with type modifications.

see the package description file here
http://hackage.haskell.org/packages/archive/json/0.7/json.cabal

In accordance with the license agreement, the full text of the license is
included here.

===BEGIN LICENSE
Copyright (c) 2007-2009 Galois Inc., All rights reserved.
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
Redistributions of source code must retain the above copyright notice, this list
of conditions and the following disclaimer. Redistributions in binary form must
reproduce the above copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials provided with the
distribution. Neither the name of the nor the names of its contributors may be
used to endorse or promote products derived from this software without specific
prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.
===END LICENSE

-}

tok              :: As3Parser a -> As3Parser a
tok p             = p <* spaces

pure   :: a -> As3Parser a
pure    = return

(<*>)  :: As3Parser (a -> b) -> As3Parser a -> As3Parser b
(<*>)   = ap

(*>)   :: As3Parser a -> As3Parser b -> As3Parser b
(*>)    = (>>)

(<*)   :: As3Parser a -> As3Parser b -> As3Parser a
m <* n  = do x <- m; n; return x

empty  :: As3Parser a
empty   = mzero

(<$>)  :: (a -> b) -> As3Parser a -> As3Parser b
(<$>)   = fmap

(<$)   :: a -> As3Parser b -> As3Parser a
x <$ m  = m >> return x
