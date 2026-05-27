{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Templates.Partials.Containers.Dropdown where

import Classh
import Classh.Reflex
import Reflex.Dom.Core
import Control.Lens ((%~), (.~))
import Control.Monad (forM, forM_, join)
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Text as T
import Text.Read (readMaybe)
import Templates.DomExtras (hideWhenAttr, showWhenAttr)
import Templates.Partials.Buttons (createNewToggleButton)
import Templates.Partials.Containers.Selection
  ( Selection (..)
  , selectionDropdownMountedOnce
  )
import Templates.Types (El, RawSelect', SelectEl, Template)


-- | TODO: take an element config for the options elements as well
-- | TODO: clean up and upstream to reflex-dom-contrib
-- dropdown'
--   :: ( MonadFix m
--      , DomBuilder t m
--      )
--   => Map.Map a T.Text
--   -> SelectElementConfig er t (DomBuilderSpace m)
--   -> m (Dynamic t a)
-- dropdown' = dropdown''
--   (only (noTransition (solidColor White)))
--   [ ("def", noTransition (color (Gray C300)))
--   , ("focus", noTransition (color (Cyan C500)))
--   ]

-- | Parameterized version with custom colors. The text color is for
-- the rendered '<select>' (i.e. the currently selected option) — pass
-- a dark colour for light backgrounds and vice versa. Browsers will
-- continue to use system defaults for the *open* options menu;
-- styling that requires platform-level overrides.
dropdown''
  :: ( MonadFix m
     , DomBuilder t m
     )
  => WhenTW (WithTransition GradientColor) -- ^ Background color
  -> WhenTW (WithTransition ColorWithOpacity) -- ^ Border color (including focus state)
  -> ColorWithOpacity -- ^ Text color (selected option in the closed dropdown)
  -> Map.Map a T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdown'' bgCol borderCol textCol options cfg' = mdo
  -- BoxConfig classes (layout, bg, border) and TextConfigTW classes
  -- (text colour) are different setter types, so they can't share a
  -- single classhUnsafe list. We assemble each separately and join.
  let boxCls  = classhUnsafe [ w .~~ TWSize_Full
                             , px .~~ TWSize 4
                             , py .~~ TWSize 3
                             , bw .~~ B1
                             , bc .~ borderCol
                             , br .~~ R_Lg
                             , border . outline .~ [("focus", Outline_None)]
                             , mb .~~ twSize' 5
                             , bgColor .~ bgCol
                             , custom .~ "font-[Sarabun] text-lg"
                             ]
      textCls = classhUnsafe [ text_color .~~ textCol ]
      class'  = boxCls <> " " <> textCls
  let safeInitial = case Map.toList options of
        [] -> error "dropdown' requires non-empty options map"
        (k,v):_ -> v
  let cfg = cfg'
            & selectElementConfig_initialValue .~ safeInitial
            & selectElementConfig_setValue .~ optionsEvent
            & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
  (selectEl, optionsEvent) <- selectElement cfg $ do
    optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
    pure $ leftmost optionsEv
  let options' = Map.fromList $ fmap flipTup $ Map.toList options
  pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_,b_) = (b_,a_)
    makeOpt optText = do
      (e, _) <- elAttr' "option" ("value" =: optText) $ text optText
      pure $ optText <$ domEvent Click e

-- dropdownWithDefault
--   :: ( MonadFix m
--      , DomBuilder t m
--      )
--   => Map.Map a T.Text
--   -> T.Text
--   -> SelectElementConfig er t (DomBuilderSpace m)
--   -> m (Dynamic t a)
-- dropdownWithDefault = dropdownWithDefault'
--   (only (noTransition (solidColor White)))
--   [ ("def", noTransition (color (Gray C300)))
--   , ("focus", noTransition (color (Cyan C500)))
--   ]

-- | Parameterized version with custom colors
dropdownWithDefault'
  :: ( MonadFix m
     , DomBuilder t m
     )
  => WhenTW (WithTransition GradientColor) -- ^ Background color
  -> WhenTW (WithTransition ColorWithOpacity) -- ^ Border color (including focus state)
  -> Map.Map a T.Text
  -> T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdownWithDefault' bgCol borderCol options start cfg' = mdo
  let class' = classhUnsafe [ w .~~ TWSize_Full
                            , px .~~ TWSize 4
                            , py .~~ TWSize 3
                            , bw .~~ B1
                            , bc .~ borderCol
                            , br .~~ R_Lg
                            , border . outline .~ [("focus", Outline_None)]
                            , mb .~~ twSize' 5
                            , bgColor .~ bgCol
                            ]
  let safeInitial = start
  let cfg = cfg'
            & selectElementConfig_initialValue .~ safeInitial
            & selectElementConfig_setValue .~ optionsEvent
            & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
  (selectEl, optionsEvent) <- selectElement cfg $ do
    optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
    pure $ leftmost optionsEv
  let options' = Map.fromList $ fmap flipTup $ Map.toList options
  pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_,b_) = (b_,a_)
    makeOpt optText = do
      (e, _) <- elAttr' "option" ("value" =: optText) $ textS (classhUnsafe [text_font .~~ Font_Custom "Sarabun", text_size .~~ LG]) optText
      pure $ optText <$ domEvent Click e

-- ============================================================================
-- Polymorphic select widgets: per-option typed values, no string parsing
-- at the call site.
-- ============================================================================

-- | A label + '<select>'. Each option's typed value @a@ is paired
-- with its '<option>' element ref so per-option click events fire
-- the typed value directly — the FRP layer just holds over
-- @leftmost . rawSelectToEventA@, with no string parsing.
--
-- Per-option click on '<option>' is unreliable across browsers
-- (Firefox / mobile pickers don't fire it on keyboard / native
-- picker selection). For prefilled dropdowns where the value must
-- track 'change' events, prefer 'labeledTypedSelectElWithPrefill'.
labeledTypedSelectEl
  :: Template t m
  => WhenTW (WithTransition GradientColor)
  -> WhenTW (WithTransition ColorWithOpacity)
  -> ColorWithOpacity
  -> ColorWithOpacity                  -- ^ label text colour
  -> T.Text                            -- ^ label
  -> [(a, T.Text)]                     -- ^ (typed value, display label)
  -> m (RawSelect' t m a)
labeledTypedSelectEl bgCol borderCol textCol labelCol label opts = do
  elAttr "label" ("class" =: classhUnsafe [mb .~~ twSize' 1, custom .~ "block", mt .~~ twSize' 4]) $
    textS (classhUnsafe [text_color .~~ labelCol, text_size .~~ SM, text_weight .~~ Medium]) label
  elAttr' "select"
    ( "class" =:
        ( classhUnsafe
            [ w .~~ TWSize_Full
            , px .~~ TWSize 4
            , py .~~ TWSize 3
            , bw .~~ B1
            , bc .~ borderCol
            , br .~~ R_Lg
            , border . outline .~ [("focus", Outline_None)]
            , bgColor .~ bgCol
            ]
          <> " "
          <> classhUnsafe [text_color .~~ textCol]
        )
    )
    $ forM opts $ \(v, lbl) -> do
      (optEl, _) <- elAttr' "option" mempty $ text lbl
      pure (optEl, v)

-- | Variant of 'labeledTypedSelectEl' that supports a prefill event
-- (for autofill flows) and returns the selected value as a
-- 'Dynamic t a'. Uses position-based option keys, so the option's
-- typed value @a@ is what flows through — never an enum-name parse.
labeledTypedSelectElWithPrefill
  :: forall t m a.
     ( Template t m, Eq a
     , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
     )
  => WhenTW (WithTransition GradientColor)
  -> WhenTW (WithTransition ColorWithOpacity)
  -> ColorWithOpacity
  -> ColorWithOpacity                  -- ^ label text colour
  -> T.Text                            -- ^ label
  -> [(a, T.Text)]                     -- ^ (typed value, display label) — first entry is the default
  -> a                                 -- ^ initial value (must be one of the option values)
  -> Event t a                         -- ^ programmatic re-selection
  -> m (Dynamic t a)
labeledTypedSelectElWithPrefill bgCol borderCol textCol labelCol label opts initial prefillEv = do
  elAttr "label" ("class" =: classhUnsafe [mb .~~ twSize' 1, custom .~ "block", mt .~~ twSize' 4]) $
    textS (classhUnsafe [text_color .~~ labelCol, text_size .~~ SM, text_weight .~~ Medium]) label
  -- Position-based key: each option's value attribute is its index
  -- in 'opts'. Mapping back from selected key to typed value is
  -- just index lookup, no enum-name Read.
  let optsIndexed = zip [0 :: Int ..] opts
      keyOf v = T.pack $ show $ maybe 0 fst $
        List.find (\(_, (val, _)) -> val == v) optsIndexed
      valOf k = case readMaybe (T.unpack k) :: Maybe Int of
        Just i  -> maybe initial fst (List.lookup i optsIndexed)
        Nothing -> initial
      initialKey  = keyOf initial
      prefillKeys = keyOf <$> prefillEv
  sel <- selectElement
    ( def
      & selectElementConfig_initialValue .~ initialKey
      & selectElementConfig_setValue     .~ prefillKeys
      & selectElementConfig_elementConfig . elementConfig_initialAttributes .~
          ( "class" =:
              ( classhUnsafe
                  [ w .~~ TWSize_Full
                  , px .~~ TWSize 4
                  , py .~~ TWSize 3
                  , bw .~~ B1
                  , bc .~ borderCol
                  , br .~~ R_Lg
                  , border . outline .~ [("focus", Outline_None)]
                  , bgColor .~ bgCol
                  ]
                <> " "
                <> classhUnsafe [text_color .~~ textCol]
              )
          )
    ) $ forM_ optsIndexed $ \(i, (_, lbl)) ->
      elAttr "option" ("value" =: T.pack (show i)) $ text lbl
  pure $ valOf <$> _selectElement_value (fst sel)

-- | A '<select>' whose option list is driven by a Dynamic. Each
-- option carries the row's @Int64@ id rendered as its HTML value;
-- the displayed text is the row's name. An always-present
-- empty-string placeholder option means "nothing picked yet".
--
-- Supports a caller-driven 'clearEv' that resets the selection
-- back to the placeholder (used, e.g., by 'selectOrCreateBlock'
-- when the user enters create-new mode).
--
-- 'initialId' is honoured ~100ms after the options Dynamic first
-- updates with a list containing that id — the delay lets the
-- inner @dyn_@ materialize the matching '<option>' before the
-- selectElement's setValue fires, otherwise the browser silently
-- falls back to the first option.
dynamicResourceSelect
  :: ( Template t m
     , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
     )
  => WhenTW (WithTransition GradientColor)
  -> WhenTW (WithTransition ColorWithOpacity)
  -> ColorWithOpacity
  -> ColorWithOpacity                  -- ^ label text colour
  -> T.Text                            -- ^ label
  -> Maybe Int64                       -- ^ pre-selected row id (Nothing → placeholder)
  -> Event t ()                        -- ^ clear-to-placeholder signal
  -> Dynamic t [(Int64, T.Text)]       -- ^ (id, displayName) options
  -> m (SelectEl t m)
dynamicResourceSelect bgCol borderCol textCol labelCol label initialId clearEv optsD = do
  elAttr "label" ("class" =: classhUnsafe [mb .~~ twSize' 1, custom .~ "block", mt .~~ twSize' 4]) $
    textS (classhUnsafe [text_color .~~ labelCol, text_size .~~ SM, text_weight .~~ Medium]) label
  let initialValue = maybe "" (T.pack . show) initialId
  -- 100ms delay so the <option> DOM rendered by the inner dyn_ has
  -- definitely materialized before we ask the select to land on
  -- initialValue. Without the delay we race with dyn_ on the same
  -- Reflex frame and the browser silently falls back to the first
  -- option (the placeholder) because no matching value exists yet.
  setValueOnOptsLoad <- delay 0.1 $ initialValue <$ updated optsD
  let setValueEv = leftmost [setValueOnOptsLoad, "" <$ clearEv]
  fmap fst $ selectElement
    ( def & selectElementConfig_initialValue .~ initialValue
          & selectElementConfig_setValue     .~ setValueEv
          & selectElementConfig_elementConfig . elementConfig_initialAttributes
              .~ ("class" =:
                   ( classhUnsafe
                       [ w .~~ TWSize_Full
                       , px .~~ TWSize 4
                       , py .~~ TWSize 3
                       , bw .~~ B1
                       , bc .~ borderCol
                       , br .~~ R_Lg
                       , border . outline .~ [("focus", Outline_None)]
                       , bgColor .~ bgCol
                       ]
                     <> " "
                     <> classhUnsafe [text_color .~~ textCol]
                   ))
    )
    $ do
      elAttr "option" ("value" =: "") $ text "(select one)"
      dyn_ $ ffor optsD $ \opts ->
        forM_ opts $ \(rowId, displayName) ->
          elAttr "option" ("value" =: T.pack (show rowId)) $ text displayName

-- | Compose a 'dynamicResourceSelect' + a 'createNewToggleButton'
-- + a caller-supplied create-form widget into the standard
-- "pick existing OR create new" UX. Two cross-influences:
--
--   1. Entering create-mode clears the dropdown to its placeholder
--      so a downstream payload reads as "no existing picked" and
--      the form's new-entity values are what the caller uses.
--   2. Picking an existing entity from the dropdown forces
--      create-mode back to False, hiding the create form — so a
--      half-filled create form can never accidentally leak into
--      the submission once the user picks an existing entity.
--
-- Returns the select element ref, the create-mode 'Dynamic'
-- (True when the toggle is on OR there are no existing options),
-- and the create-form widget's result.
selectOrCreateBlock
  :: forall t m formRefs k.
     ( Template t m, MonadFix m
     , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
     , Adjustable t m, Eq k
     )
  => [BoxConfig -> BoxConfig]          -- ^ box style for the dropdown
  -> ColorWithOpacity                  -- ^ select element text colour
  -> ColorWithOpacity                  -- ^ label + button text colour
  -> T.Text                            -- ^ entity label (e.g. "team")
  -> Maybe k                           -- ^ pre-selected existing-entity id (EditJob); Nothing for new
  -> Dynamic t [(k, T.Text)]           -- ^ existing options (Dynamic to support late-arriving API data)
  -> m formRefs                        -- ^ create-form widget
  -> m (Dynamic t (Maybe k), Dynamic t Bool, formRefs)
selectOrCreateBlock boxStyle textCol labelCol entityLabel initialId optsD createFormWidget = mdo
  let hasOptsD :: Dynamic t Bool
      hasOptsD = not . null <$> optsD
      -- Build a Selection from the options Dynamic. Initial id (if
      -- present) goes at the head so the dropdown mounts on the
      -- already-saved selection. Empty list → no Selection → the
      -- mountedOnce widget stays unmounted.
      selectionEv :: Event t (Selection (Maybe k))
      selectionEv = fmapMaybe id $ ffor (updated optsD) $ \opts -> case opts of
        [] -> Nothing
        _  -> Just $ buildSelection initialId opts
      -- Picking an existing entity = the dropdown's value becomes
      -- @Just (Just _)@ — outer Just = "mounted", inner Just =
      -- "real selection (not placeholder)".
      pickedExistingEv :: Event t ()
      pickedExistingEv = () <$ fmapMaybe id (updated (join <$> selectedD))
      -- Entering create-mode resets the dropdown to its placeholder
      -- (the @Nothing@ head). Rising edge of createModeD only.
      clearOnCreate :: Event t (Maybe k)
      clearOnCreate = Nothing <$ ffilter id (updated createModeD)
  selectedD <- elDynAttr "div" (hideWhenAttr (not <$> hasOptsD)) $
    selectionDropdownMountedOnce
      boxStyle
      textCol
      (Just $ elAttr "label" ("class" =: classhUnsafe [mb .~~ twSize' 1, custom .~ "block", mt .~~ twSize' 4]) $
        textS (classhUnsafe [text_color .~~ labelCol, text_size .~~ SM, text_weight .~~ Medium])
          ("Existing " <> entityLabel))
      clearOnCreate
      mempty
      selectionEv
  toggleClickEv <- elDynAttr "div" (hideWhenAttr (not <$> hasOptsD)) $
    createNewToggleButton labelCol entityLabel toggleD
  -- Toggle's user-controlled state. Click flips; picking an
  -- existing resets to False so the submission drops the
  -- new-form values.
  toggleD <- foldDyn ($) False $ leftmost
    [ not          <$ toggleClickEv
    , const False  <$ pickedExistingEv
    ]
  let createModeD = (\hasOpts on -> not hasOpts || on) <$> hasOptsD <*> toggleD
      -- selectionDropdownMountedOnce returns Dynamic t (Maybe (Maybe Int64));
      -- 'join' collapses "not-yet-mounted" and "placeholder" into Nothing.
      pickedD = join <$> selectedD
  formRefs <- elDynAttr "div" (showWhenAttr createModeD) createFormWidget
  pure (pickedD, createModeD, formRefs)
  where
    -- Build a non-empty Selection from options + optional initial id:
    --   * If 'initialId' matches a row, that row is the Selection
    --     head — the dropdown mounts with it already selected. The
    --     placeholder is demoted to a selectable alternative so the
    --     user can still "unselect" if they want.
    --   * If 'initialId' is Nothing (or doesn't match any row), the
    --     placeholder is the head and the dropdown mounts on
    --     "(select one)".
    buildSelection :: Maybe k -> [(k, T.Text)] -> Selection (Maybe k)
    buildSelection mInit opts =
      let placeholder = (Nothing, "(select one)")
      in case mInit >>= \iid -> List.find (\(i, _) -> i == iid) opts of
           Just (iid, lbl) ->
             let rest        = filter (\(i, _) -> i /= iid) opts
                 restAsMaybe = map (\(i, t) -> (Just i, t)) rest
             in Selection (Just iid, lbl) (placeholder : restAsMaybe)
           Nothing ->
             Selection placeholder (map (\(i, t) -> (Just i, t)) opts)

-- | Parse the raw '<select>' value (a 'Text') to 'Maybe Int64'.
-- The select renders rowIds via 'show', so 'readMaybe' round-trips.
-- Empty / unparseable → 'Nothing' (i.e. placeholder).
parseSelectedId :: Reflex t => Dynamic t T.Text -> Dynamic t (Maybe Int64)
parseSelectedId = fmap parseInt64

parseInt64 :: T.Text -> Maybe Int64
parseInt64 t
  | T.null (T.strip t) = Nothing
  | otherwise          = readMaybe (T.unpack (T.strip t))
