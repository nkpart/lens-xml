{-|
Module      : Text.XML.Light.Lens
Description : Lenses for xml
Copyright   : (c) Nick Partridge, 2016
License     : BSD-3
Maintainer  : nkpart@gmail.com

This module defines lenses and prisms for @Text.XML.Light@. The naming should be consistent:

* Prisms: an underscore followed by the name of the constructor they wrap.
* Lenses: To avoid clashes with Text.XML.Light, lenses are named as for the record field, with a suffix of `L`.
-}

module Text.XML.Light.Lens (
  -- * Content
  _Elem, _Text, _CRef,
  -- * Elem
  elNameL, elAttribsL, elContentL, elLineL,
  -- * Attr
  attrKeyL, attrValL,
  -- * CData
  cdVerbatimL, cdDataL, cdLineL,
  -- * CDataKind
  _CDataText, _CDataVerbatim, _CDataRaw,
  -- * QName
  qNameL, qURIL, qPrefixL,
                           ) where

import Control.Lens
import Text.XML.Light

-- |
_Elem :: Prism' Content Element
_Elem = prism Elem g
  where g (Elem e) = Right e
        g v = Left v

-- |
_Text :: Prism' Content CData
_Text = prism Text g
  where g (Text e) = Right e
        g v = Left v

-- |
_CRef :: Prism' Content String
_CRef = prism CRef g
  where g (CRef e) = Right e
        g v = Left v

-- | 
elNameL :: Lens' Element QName
elNameL = lens elName (\v a -> v { elName = a })

elAttribsL :: Lens' Element [Attr]
elAttribsL = lens elAttribs (\v a -> v { elAttribs = a })

elContentL :: Lens' Element [Content]
elContentL = lens elContent (\v a -> v { elContent = a })

elLineL :: Lens' Element (Maybe Line)
elLineL = lens elLine (\v a -> v { elLine = a })

attrKeyL :: Lens' Attr QName
attrKeyL = lens attrKey (\v a -> v { attrKey = a })

attrValL :: Lens' Attr String
attrValL = lens attrVal (\v a -> v { attrVal = a })

cdVerbatimL :: Lens' CData CDataKind
cdVerbatimL = lens cdVerbatim (\v a -> v { cdVerbatim = a })

cdDataL :: Lens' CData String
cdDataL = lens cdData (\v a -> v { cdData = a })

cdLineL :: Lens' CData (Maybe Line)
cdLineL = lens cdLine (\v a -> v { cdLine = a })

_CDataText :: Prism' CDataKind ()
_CDataText = prism' (const CDataText) g
  where g CDataText = Just ()
        g _ = Nothing

_CDataVerbatim :: Prism' CDataKind ()
_CDataVerbatim = prism' (const CDataVerbatim) g
  where g CDataVerbatim = Just ()
        g _ = Nothing

_CDataRaw :: Prism' CDataKind ()
_CDataRaw = prism' (const CDataRaw) g
  where g CDataRaw = Just ()
        g _ = Nothing

qNameL :: Lens' QName String
qNameL = lens qName (\v a -> v { qName = a })

qURIL :: Lens' QName (Maybe String)
qURIL = lens qURI (\v a -> v { qURI = a })

qPrefixL :: Lens' QName (Maybe String)
qPrefixL = lens qPrefix (\v a -> v { qPrefix = a })
