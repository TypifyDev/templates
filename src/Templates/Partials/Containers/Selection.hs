{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A 'Selection' is a non-empty list of options with one option
-- designated as currently-selected. The widget functions render a
-- styled @\<select\>@ from a 'Selection' and return both the element
-- ref and the user's current pick as a 'Dynamic'.
--
-- Design priorities for downstream library users:
--
--   * The 'Selection' type itself is pure data, no theming.
--   * The widget takes layout / styling / label / set-value / extra
--     HTML attributes as explicit arguments so callers control the
--     full visual surface. Nothing about a specific theme is baked
--     in.
--   * Index-based HTML @value@ attributes mean the typed @a@ never
--     has to be 'Show'/'Read' — round-trip is a position lookup.
--   * The "placeholder" convention is offered as a helper
--     ('selectionWithPlaceholder') that wraps option values in
--     'Maybe', not as a special case in the widget.
module Templates.Partials.Containers.Selection
  ( -- * The data type
    Selection (..)
  , selectionInitial
  , selectionOptions
  , selectionWithPlaceholder
  , selectionFromList
    -- * Widgets
  , selectionDropdown
  , selectionDropdownMountedOnce
  ) where

import Classh
import Reflex.Dom.Core hiding (El)
import Control.Monad (forM_, join)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Read (readMaybe)
import Templates.Types (SelectEl, Template)

-- | Non-empty options list with the head designated as
-- currently-selected.
--
-- @Selection (selectedVal, selectedLabel) [(altVal, altLabel), ...]@
--
-- The label is the human-readable text shown inside the
-- @\<option\>@; the value is whatever typed value the caller wants
-- back from the user's pick. The widget renders options in the
-- order @selected : alternatives@.
data Selection a = Selection (a, T.Text) [(a, T.Text)]
  deriving (Eq, Show, Functor)

-- | The initially-selected value.
selectionInitial :: Selection a -> a
selectionInitial (Selection (a, _) _) = a

-- | All options in render order: selected first, then alternatives.
selectionOptions :: Selection a -> [(a, T.Text)]
selectionOptions (Selection h rest) = h : rest

-- | Build a 'Selection' from a list by taking the head as the
-- selected element. 'Nothing' on an empty list — callers needing a
-- "this list is always non-empty" guarantee should construct
-- 'Selection' directly.
selectionFromList :: [(a, T.Text)] -> Maybe (Selection a)
selectionFromList []         = Nothing
selectionFromList (head':rs) = Just (Selection head' rs)

-- | Convenience: build a @Selection (Maybe a)@ where 'Nothing' is
-- the head with the given placeholder label, and 'Just'-wrapped
-- alternatives follow. Use when the "no real choice yet" state
-- should be selectable, e.g. a "(select one)" entry.
--
-- @
-- selectionWithPlaceholder "(select one)"
--   [ (Frontend, "Frontend Engineer"), (Backend, "Backend Engineer") ]
-- -- =>
-- -- Selection (Nothing, "(select one)")
-- --   [ (Just Frontend, "Frontend Engineer")
-- --   , (Just Backend,  "Backend Engineer")
-- --   ]
-- @
selectionWithPlaceholder :: T.Text -> [(a, T.Text)] -> Selection (Maybe a)
selectionWithPlaceholder placeholderLabel rest =
  Selection (Nothing, placeholderLabel)
            (map (\(v, l) -> (Just v, l)) rest)

-- | Render a @\<select\>@ from a 'Selection'. Returns both the
-- outer element ref (for callers needing to attach events / focus
-- it / etc.) and a 'Dynamic' carrying the user's current pick.
--
-- All visual / behaviour knobs are explicit arguments:
--
--   * @boxStyle@ — list of 'BoxConfig' setters for the select's
--     layout, background, border, etc. Nothing about a specific
--     theme is hard-coded — caller decides full-width vs not,
--     padding, border-radius, focus styles, gradients, the lot.
--   * @textColour@ — colour of the rendered closed-select text
--     (the open-options menu uses browser defaults regardless).
--   * @mLabel@ — optional widget to render above the select (e.g.
--     a styled @\<label\>@). 'Nothing' renders no label.
--   * @setValueEv@ — programmatic re-selection. Pass 'never' if
--     not needed; otherwise each occurrence updates the rendered
--     selection.
--   * @extraAttrs@ — extra HTML attributes to merge onto the
--     @\<select\>@ (id, data-*, aria-*, etc.). The @class@
--     attribute is built from @boxStyle@ + @textColour@ — if
--     @extraAttrs@ also contains @class@, the caller's value
--     wins.
selectionDropdown
  :: forall t m a.
     ( Template t m, Eq a )
  => [BoxConfig -> BoxConfig]                    -- ^ boxStyle
  -> ColorWithOpacity                            -- ^ textColour
  -> Maybe (m ())                                -- ^ mLabel
  -> Event t a                                   -- ^ setValueEv
  -> Map.Map AttributeName T.Text                -- ^ extraAttrs (merged onto the @<select>@; caller's @class@ wins if supplied)
  -> Selection a
  -> m (SelectEl t m, Dynamic t a)
selectionDropdown boxStyle textColour mLabel setValueEv extraAttrs selection = do
  maybe (pure ()) id mLabel
  let opts         = selectionOptions selection
      initial      = selectionInitial selection
      optsIndexed  = zip [0 :: Int ..] opts
      -- Encode each option's typed value as its position. Lookup
      -- round-trips for the lifetime of this widget.
      keyOf v = T.pack $ show $ maybe 0 fst $
        List.find (\(_, (val, _)) -> val == v) optsIndexed
      valOf k = case readMaybe (T.unpack k) :: Maybe Int of
        Just i  -> maybe initial fst (List.lookup i optsIndexed)
        Nothing -> initial
      classAttr = classhUnsafe boxStyle <> " " <> classhUnsafe [text_color .~~ textColour]
      -- Caller's extraAttrs take precedence over the computed
      -- class — but if caller didn't supply class, the computed
      -- one stays. Map.union is left-biased.
      attrs = Map.union extraAttrs ("class" =: classAttr)
  (sel, _) <- selectElement
    ( def & selectElementConfig_initialValue .~ keyOf initial
          & selectElementConfig_setValue     .~ (keyOf <$> setValueEv)
          & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs
    ) $ forM_ optsIndexed $ \(i, (_, lbl)) ->
      elAttr "option" ("value" =: T.pack (show i)) $ text lbl
  pure (sel, valOf <$> _selectElement_value sel)

-- | Variant of 'selectionDropdown' for when the 'Selection' comes
-- from an asynchronous source (e.g. a single API response). The
-- widget mounts on the FIRST occurrence of the source event and
-- ignores subsequent events — this preserves any in-progress user
-- selection. If you genuinely want a rebuild on every event,
-- wire @widgetHold@ + 'selectionDropdown' yourself.
--
-- Before the first event fires, the inner 'Dynamic' is 'Nothing'
-- and no widget is rendered. After it fires, the 'Dynamic' is
-- 'Just' the user's current pick.
selectionDropdownMountedOnce
  :: forall t m a.
     ( Template t m, Eq a, Adjustable t m )
  => [BoxConfig -> BoxConfig]
  -> ColorWithOpacity
  -> Maybe (m ())
  -> Event t a
  -> Map.Map AttributeName T.Text
  -> Event t (Selection a)
  -> m (Dynamic t (Maybe a))
selectionDropdownMountedOnce boxStyle textColour mLabel setValueEv extraAttrs sourceEv = do
  firstEv <- headE sourceEv
  innerDD <- widgetHold (pure (constDyn Nothing)) $
    ffor firstEv $ \selection -> do
      (_, valD) <- selectionDropdown boxStyle textColour mLabel setValueEv extraAttrs selection
      pure (Just <$> valD)
  pure (join innerDD)
