{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Buttons where

import Templates.Types
import Classh as C
import Classh.Reflex as C
import Data.Text as T
import Reflex.Dom.Core
import Control.Lens ((%~), (.~))
import Data.Proxy
import Data.Char (isAlphaNum, isSpace)


-- sendButton :: DomBuilder t m => m (Event t ())
-- sendButton = iconButton "send"

iconButton'
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color (with states)
  -> C.WhenTW C.ColorWithOpacity                   -- ^ Text/icon color
  -> Text                                          -- ^ Icon name
  -> m (Event t ())
iconButton' bgCol txtCol icon = do
  (e, _) <- elClass' "button" classes $ C.textS textCfg icon
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe
      [ C.text_color .~ txtCol
      , C.custom .~ "font-icon text-icon leading-none"
      ]
    classes = C.classhUnsafe
      [ C.bgColor .~ bgCol
      , C.br .~~ C.R_Normal
      , C.p .~~ C.TWSize 2.5
      , C.w .~~ C.pct 50
      , C.h .~~ C.pct 95
      , C.shadow .~~ C.Shadow_Md
      , C.border . C.outline .~ [("focus", C.Outline_None)]
      ]

iconButtonEnabled'
  :: (DomBuilder t m, PostBuild t m)
  => C.GradientColor      -- ^ Background color
  -> C.ColorWithOpacity   -- ^ Icon color
  -> Dynamic t Bool
  -> Text
  -> m (Event t ())
iconButtonEnabled' bgCol iconCol enabled icon = do
  let attrs = ffor enabled $ \b ->
        if b
        then "class" =: classes
        else "class" =: classes <> "disabled" =: "1"
  (e, _) <- elDynAttr' "button" attrs $ C.textS textCfg icon
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe
      [ C.text_color .~~ iconCol
      , C.custom .~ "font-icon text-icon leading-none"
      ]
    classes = C.classhUnsafe
      [ C.bgColor .~~ bgCol
      , C.br .~~ C.R_Normal
      , C.p .~~ C.TWSize 2.5
      , C.shadow .~~ C.Shadow_Md
      , C.border . C.outline .~ [("focus", C.Outline_None)]
      , C.custom .~ "flex-shrink-0"
      ]



-- primaryButtonDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m (Event t ())
-- primaryButtonDyn = primaryButtonDyn'
--   (C.only (C.noTransition (C.solidColor (C.Violet C.C600))))  -- bg
--   [("hover", C.solidColor (C.Violet C.C700) `C.withTransition` C.Duration_200)
--   ,("active", C.solidColor (C.Violet C.C500) `C.withTransition` C.Duration_100)]  -- bg states
--   (C.only (C.color C.White))  -- text
--   C.Shadow_Md  -- shadow
--   [("focus", C.noTransition C.Ring_4)]  -- ring
--   (C.color (C.Violet C.C500))  -- ring color

primaryButtonDyn'
  :: (DomBuilder t m, PostBuild t m)
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.WhenTW C.ColorWithOpacity                   -- ^ Text color
  -> C.BoxShadow                                   -- ^ Shadow
  -> [(C.TWCondition, C.WithTransition C.RingWidth)]  -- ^ Ring width states
  -> C.ColorWithOpacity                            -- ^ Ring color
  -> Dynamic t Bool
  -> Text
  -> m (Event t ())
primaryButtonDyn' bgCol bgStates txtCol shadowVal ringStates ringCol enabled buttonText = do
  let attrs = ffor enabled $ \b -> if b then "class" =: classes else "class" =: classes <> "disabled" =: "1"
  (e, _) <- elDynAttr' "button" attrs $ C.textS textCfg buttonText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe
      [ C.text_color .~ txtCol
      , C.text_weight .~~ C.Bold
      , C.custom .~ "font-facit text-body text-center"
      ]
    classes = C.classhUnsafe
      [ C.bgColor .~ bgCol
      , C.bgColor .~^ bgStates
      , C.w .~~ C.TWSize_Full
      , C.p .~~ C.TWSize 4
      , C.mt .~~ C.twSize' 12
      , C.shadow .~~ shadowVal
      , C.br .~~ C.R_Normal
      , C.border . C.outline .~ [("focus", C.Outline_None)]
      , C.border . C.ring . C.ringWidth .~ ringStates
      , C.border . C.ring . C.ringColor .~~ ringCol
      , C.border . C.ring . C.ringOpacity .~~ 50
      ]

secondaryIconButton
  :: DomBuilder t m
  => C.GradientColor      -- ^ Background color
  -> C.ColorWithOpacity   -- ^ Icon color
  -> C.ColorWithOpacity   -- ^ Border color
  -> C.ColorWithOpacity   -- ^ Ring color
  -> Text                 -- ^ Extra classes
  -> Text                 -- ^ Icon name
  -> m (Event t ())
secondaryIconButton bgCol iconCol borderCol ringCol cs icon = do
  (e, _) <- elClass' "button" classes $
    elClass "div" iconClasses $ C.textS iconTextCfg icon
  pure $ domEvent Click e
  where
    iconTextCfg = C.classhUnsafe [ C.text_color .~~ iconCol ]
    iconClasses = C.classhUnsafe [ C.box_custom .~ "font-icon leading-none text-icon" ]
    classes = C.classhUnsafe
      [ C.bgColor .~~ bgCol
      , C.bc .~~ borderCol
      , C.bw .~~ C.B1
      , C.br .~~ C.R_Normal
      , C.p .~~ C.TWSize 2.5
      , C.border . C.outline .~ [("focus", C.Outline_None)]
      , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
      , C.border . C.ring . C.ringColor .~~ ringCol
      , C.border . C.ring . C.ringOpacity .~~ 50
      , C.custom .~ ("flex-shrink-0 " <> cs)
      ]


secondaryButton
  :: DomBuilder t m
  => C.GradientColor      -- ^ Background color
  -> C.ColorWithOpacity   -- ^ Text color
  -> C.ColorWithOpacity   -- ^ Border color
  -> C.ColorWithOpacity   -- ^ Ring color
  -> Text                 -- ^ Extra classes
  -> Text                 -- ^ Label
  -> m (Event t ())
secondaryButton bgCol txtCol borderCol ringCol cs label = do
  (e, _) <- elClass' "button" classes $ C.textS textCfg label
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe
      [ C.text_color .~~ txtCol
      , C.text_weight .~~ C.Bold
      , C.custom .~ "font-facit leading-none text-center"
      ]
    classes = C.classhUnsafe
      [ C.bgColor .~~ bgCol
      , C.bc .~~ borderCol
      , C.bw .~~ C.B1
      , C.br .~~ C.R_Normal
      , C.w .~~ C.TWSize_Full
      , C.p .~~ C.TWSize 2.5
      , C.border . C.outline .~ [("focus", C.Outline_None)]
      , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
      , C.border . C.ring . C.ringColor .~~ ringCol
      , C.border . C.ring . C.ringOpacity .~~ 50
      , C.custom .~ cs
      ]


-- navyBlueButton :: DomBuilder t m => Text -> m (Event t ())
-- navyBlueButton = navyBlueButton'
--   (C.only (C.noTransition (C.solidColor (C.Violet C.C600))))  -- violet for visibility on dark bg
--   [("hover", C.solidColor (C.Violet C.C500) `C.withTransition` C.Duration_200)
--   ,("active", C.solidColor (C.Violet C.C400) `C.withTransition` C.Duration_100)]
--   (C.only (C.color C.White))
--   (C.color (C.Violet C.C300))  -- ring color

navyBlueButton'
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.WhenTW C.ColorWithOpacity  -- ^ Text color
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text
  -> m (Event t ())
navyBlueButton' bgCol bgStates txtCol ringCol buttonText = do
  (e, _) <- elClass' "button" boxCfg $ C.textS textCfg buttonText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe [ C.text_color .~ txtCol
                             , C.text_font .~~ C.Font_Custom "Sarabun"
                             , C.text_weight .~~ C.Bold
                             ]
    boxCfg = C.classhUnsafe [ C.w .~~ C.TWSize_Full
                            , C.p .~~ C.TWSize 4
                            , C.bgColor .~ bgCol
                            , C.bgColor .~^ bgStates
                            , C.br .~~ C.R_3Xl
                            , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                            , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                            ]
                            , C.border . C.outline .~ [("focus", C.Outline_None)]
                            , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                            , C.border . C.ring . C.ringColor .~~ ringCol
                            , C.border . C.ring . C.ringOpacity .~~ 50
                            , C.transform . C.scale .~^ [("hover", C.Scale_105 `C.withTransition` C.Duration_200)
                                                        ,("active", C.Scale_95 `C.withTransition` C.Duration_100)]
                            ]




-- FROM ACE

primaryButtonImageDyn
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Dynamic t Text
  -> Height
  -> Width
  -> m (Event t ())
primaryButtonImageDyn bgCol bgStates ringCol dynImageHref height width = do
  (e, _) <- elClass' "button" boxCfg $
    elDynAttr "img" imgAttrs blank
  pure $ domEvent Click e
  where
    otherImgAttrs = "height" =: height <> "width" =: width <> "class" =: "block mx-auto"
    imgAttrs = (\src -> otherImgAttrs <> "src" =: src) <$> dynImageHref
    boxCfg = C.classhUnsafe [ C.w .~~ C.TWSize_Full
                            , C.p .~~ C.TWSize 4
                            , C.mt .~~ C.twSize' 16
                            , C.bgColor .~ bgCol
                            , C.bgColor .~^ bgStates
                            , C.br .~~ C.R_Xl
                            , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                            , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                            ]
                            , C.border . C.outline .~ [("focus", C.Outline_None)]
                            , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                            , C.border . C.ring . C.ringColor .~~ ringCol
                            , C.border . C.ring . C.ringOpacity .~~ 50
                            , C.transform . C.scale .~^ [("hover", C.Scale_105 `C.withTransition` C.Duration_200)
                                                        ,("active", C.Scale_95 `C.withTransition` C.Duration_100)]
                            , C.custom .~ "font-[Sarabun] font-bold text-white text-body text-center"
                            ]



type Height = Text
type Width = Text
-- primaryButtonImage :: DomBuilder t m => Text -> Height -> Width -> m (Event t ())
-- primaryButtonImage = primaryButtonImage'
--   (C.only (C.noTransition (C.solidColor (C.Cyan C.C500))))
--   [("hover", C.solidColor (C.Cyan C.C600) `C.withTransition` C.Duration_200)
--   ,("active", C.solidColor (C.Cyan C.C400) `C.withTransition` C.Duration_100)]
--   (C.color (C.Cyan C.C500))

primaryButtonImage'
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text -> Height -> Width -> m (Event t ())
primaryButtonImage' bgCol bgStates ringCol imageHref height width = do
  (e, _) <- elClass' "button" (primaryButtonBoxCfg bgCol bgStates ringCol) $
    elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
  pure $ domEvent Click e

primaryButtonImageText
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Text color
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text -> Height -> Width -> Text -> m (Event t ())
primaryButtonImageText bgCol bgStates txtCol ringCol imageHref height width bottomText = do
  (e, _) <- elClass' "button" (primaryButtonBoxCfg bgCol bgStates ringCol) $ do
    elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
    C.textS textCfg bottomText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe [ C.text_color .~~ txtCol
                             , C.text_font .~~ C.Font_Custom "Sarabun"
                             , C.text_weight .~~ C.Bold
                             ]

primaryButtonBoxCfg
  :: C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text
primaryButtonBoxCfg bgCol bgStates ringCol = C.classhUnsafe
  [ C.w .~~ C.TWSize_Full
  , C.p .~~ C.TWSize 4
  , C.mt .~~ C.twSize' 16
  , C.bgColor .~ bgCol
  , C.bgColor .~^ bgStates
  , C.br .~~ C.R_Xl
  , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                  , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                  ]
  , C.border . C.outline .~ [("focus", C.Outline_None)]
  , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
  , C.border . C.ring . C.ringColor .~~ ringCol
  , C.border . C.ring . C.ringOpacity .~~ 50
  , C.transform . C.scale .~^ [("hover", C.Scale_105 `C.withTransition` C.Duration_200)
                              ,("active", C.Scale_95 `C.withTransition` C.Duration_100)]
  , C.custom .~ "text-center"
  ]


-- primaryButtonSized :: DomBuilder t m => TWSize -> TWSize -> Text -> m (Event t ())
-- primaryButtonSized = primaryButtonSized'
--   (C.only (C.noTransition (C.solidColor (C.Cyan C.C500))))
--   [("hover", C.solidColor (C.Cyan C.C600) `C.withTransition` C.Duration_200)
--   ,("active", C.solidColor (C.Cyan C.C400) `C.withTransition` C.Duration_100)]
--   (C.color C.White)
--   (C.color (C.Cyan C.C500))

primaryButtonSized'
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Text color
  -> C.ColorWithOpacity  -- ^ Ring color
  -> TWSize -> TWSize -> Text -> m (Event t ())
primaryButtonSized' bgCol bgStates txtCol ringCol height width buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classCfg <> "name" =: name) $
    C.textS textCfg buttonText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe [ C.text_color .~~ txtCol
                             , C.text_font .~~ C.Font_Custom "Sarabun"
                             , C.text_weight .~~ C.Bold
                             , C.text_size .|~ [C.XS, C.XS, C.LG]  -- text-xs on mobile, text-lg on md+
                             ]
    classCfg = primaryClass bgCol bgStates ringCol <&> C.classhUnsafe [C.py .~~ height, C.px .~~ width]
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText


-- primaryButton :: DomBuilder t m => Text -> m (Event t ())
-- primaryButton = primaryButton'
--   (C.only (C.noTransition (C.solidColor (C.Cyan C.C500))))
--   [("hover", C.solidColor (C.Cyan C.C600) `C.withTransition` C.Duration_200)
--   ,("active", C.solidColor (C.Cyan C.C400) `C.withTransition` C.Duration_100)]
--   (C.color C.White)
--   (C.color (C.Cyan C.C500))

primaryButton'
  :: DomBuilder t m
  => C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Text color
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text -> m (Event t ())
primaryButton' bgCol bgStates txtCol ringCol buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classCfg <> "name" =: name) $
    C.textS textCfg buttonText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe [ C.text_color .~~ txtCol
                             , C.text_font .~~ C.Font_Custom "Sarabun"
                             , C.text_weight .~~ C.Bold
                             , C.text_size .|~ [C.XS, C.XS, C.LG]  -- text-xs on mobile, text-lg on md+
                             ]
    classCfg = primaryClass bgCol bgStates ringCol <&> C.classhUnsafe [C.py .~~ C.TWSize 4, C.px .~~ C.TWSize 8]
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText

primaryClass
  :: C.WhenTW (C.WithTransition C.GradientColor)  -- ^ Background color
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]  -- ^ Background hover/active states
  -> C.ColorWithOpacity  -- ^ Ring color
  -> Text
primaryClass bgCol bgStates ringCol = C.classhUnsafe
  [ C.bgColor .~ bgCol
  , C.bgColor .~^ bgStates
  , C.br .~~ C.R_Xl
  , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                  , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                  ]
  , C.border . C.outline .~ [("focus", C.Outline_None)]
  , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
  , C.border . C.ring . C.ringColor .~~ ringCol
  , C.border . C.ring . C.ringOpacity .~~ 50
  , C.transform . C.scale .~^ [("hover", C.Scale_105 `C.withTransition` C.Duration_200)
                              ,("active", C.Scale_95 `C.withTransition` C.Duration_100)]
  , C.custom .~ "text-center whitespace-nowrap inline-block"
  ]

example :: Template t m => m (Event t ())
example = buttonToggleBody "" True $ \case
  True -> text "hey"
  False -> text "Hello"

-- | Toggles between 2 inner bodies based on its clicks
buttonToggleBody :: Template t m => Dynamic t T.Text -> Bool -> (Bool -> m a) -> m (Event t ())
buttonToggleBody buttonClasses start dynDom = mdo
  (e, _) <- elDynClass' "button" buttonClasses $ dyn $ ffor booly $ dynDom
  let clk = domEvent Click e
  booly <- toggle start clk
  pure clk


-- | Stop event propogation on click
greedyButton :: forall t m a. DomBuilder t m => m a -> m (Event t (), a)
greedyButton c = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
  (e, a) <- element "button" cfg c
  return (domEvent Click e, a)

greedyButton_ :: forall t m a. DomBuilder t m => m a -> m (Event t ())
greedyButton_ c = fmap fst $ greedyButton c

-- | A "+ Create new <entity>" / "Cancel" toggle button. The
-- current on/off state is owned by the caller (passed as a
-- 'Dynamic t Bool') so external events — e.g. the user picking
-- an existing entity from a companion dropdown — can reset it.
-- Returns the click event so the caller can fold it into the
-- state Dynamic.
--
-- Label: "Cancel" when on, "+ Create new <entityLabel>" when off.
createNewToggleButton
  :: (Template t m, DomBuilder t m)
  => C.ColorWithOpacity   -- ^ text colour
  -> T.Text               -- ^ entity label (e.g. "team")
  -> Dynamic t Bool       -- ^ caller-owned on/off state (controls label only)
  -> m (Event t ())
createNewToggleButton textCol entityLabel isOnD = do
  (btnEl, _) <- elAttr' "button"
    ( "type" =: "button"
   <> "class" =: C.classhUnsafe
        [ C.mt .~~ C.twSize' 2
        , C.mb .~~ C.twSize' 2
        , C.py .~~ C.TWSize 1
        , C.px .~~ C.TWSize 3
        ]
    ) $ C.dynTextS (C.classhUnsafe [C.text_color .~~ textCol, C.text_size .~~ C.SM]) $
      ffor isOnD $ \on ->
        if on then "Cancel" else "+ Create new " <> entityLabel
  pure (domEvent Click btnEl)

-- | Companion to 'createNewToggleButton' for the "edit this existing
-- one" flow added F11 (2026-10-30). Mini-sized button rendered next
-- to the entity dropdown; parent widget hides/shows it based on
-- "is an existing entity picked AND we're not in create-mode".
editButton
  :: (Template t m, DomBuilder t m)
  => C.WhenTW (C.WithTransition C.GradientColor)
                              -- ^ background colour (same shape as primaryButton')
  -> [(C.TWCondition, C.WithTransition C.GradientColor)]
                              -- ^ background hover/active states
  -> C.ColorWithOpacity        -- ^ text colour
  -> C.ColorWithOpacity        -- ^ ring colour (focus halo)
  -> T.Text                    -- ^ entity label (unused, kept for a11y hooks)
  -> m (Event t ())
editButton bgCol bgStates txtCol ringCol _entityLabel =
  primaryButton' bgCol bgStates txtCol ringCol "\9998 Edit"
