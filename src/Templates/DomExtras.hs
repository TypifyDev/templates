{-# Language OverloadedStrings #-}
module Templates.DomExtras where

import qualified Data.Map as Map
import Reflex.Dom.Core
import Data.Text as T

elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text
        -> m a
        -> m a
elStyle etag styleString inner = elAttr etag ("style" =: styleString) inner

elDynStyle :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t T.Text -> m a -> m a
elDynStyle etag styleString inner = elDynAttr etag ((=:) "style" <$> styleString) inner

-- | Attribute map that toggles between empty and @style="display:none"@.
-- Pair with 'elDynAttr' to show/hide a subtree without re-running the
-- inner widget. The widget always builds (refs are stable); only its
-- visibility changes.
--
-- 'True'  → visible (no style override)
-- 'False' → hidden  (display: none)
showWhenAttr :: Reflex t => Dynamic t Bool -> Dynamic t (Map.Map T.Text T.Text)
showWhenAttr d = ffor d $ \visible ->
  if visible then mempty else "style" =: "display:none"

-- | Inverse of 'showWhenAttr' — 'True' → hidden, 'False' → visible.
hideWhenAttr :: Reflex t => Dynamic t Bool -> Dynamic t (Map.Map T.Text T.Text)
hideWhenAttr d = ffor d $ \hidden ->
  if hidden then "style" =: "display:none" else mempty
