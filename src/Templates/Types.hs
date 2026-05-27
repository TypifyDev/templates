{-# LANGUAGE OverloadedStrings #-}
module Templates.Types
  ( module Templates.Types
  ) where

import Reflex.Dom.Core hiding (El)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)

type Template t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

type TextAreaEl t m = TextAreaElement EventResult (DomBuilderSpace m) t 

type El t m = Element EventResult (DomBuilderSpace m) t

type SelectEl t m = SelectElement EventResult (DomBuilderSpace m) t

-- | An element paired with the typed value it represents (e.g. an
-- '<option>' tagged with the enum value it stands for). Used as the
-- building block of polymorphic selects and checkbox-lists where
-- clicks on the element should fire the typed value directly,
-- avoiding any string parsing.
type TaggedEl t m a = (El t m, a)

-- | A list of 'TaggedEl's — typically every '<option>' inside a
-- '<select>' or every checkbox in a list, each carrying its typed
-- value.
type TaggedOptions t m a = [TaggedEl t m a]

-- | A polymorphic '<select>': the outer element ref plus the
-- per-option tagged refs. Useful when you need both the select
-- itself (e.g. for reading '_selectElement_value') and the
-- per-option click events.
type RawSelect' t m a = (El t m, TaggedOptions t m a)

type ImgSrc = Text
