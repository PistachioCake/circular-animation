{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Control.Lens

data Model = Model {_time :: Float, _angles :: [Float], _num :: Float}
data Dot = Dot {_pos :: (Float, Float), _col :: Color}
makeLenses ''Model
makeLenses ''Dot

main :: IO ()
main = simulate window bkg fps initialState render update
    where
        window = InWindow "Animation" (300, 300) (300, 300)
        bkg = red
        fps = 60

initialState = Model 0 [0, pi] 1

addMore :: [Float] -> [Float]
addMore (a:bs@(b:cs)) = a:((a+b)/2):(addMore bs)
addMore [a] = [a]
addMore a = error $ "WHAT? " <> show a

update :: ViewPort -> Float -> Model -> Model
-- update _ elapsed model = ((time %~ (+elapsed)) . if elapsed + model^.time > 2*pi*(model^.num) then (angles %~ addMore) . (num %~ (+1)) else id) model
update _ elapsed model = 
    let model' = time %~ (+elapsed) $ model
    in  if model'^.time < 2*pi*(model^.num) then model' else
        (angles %~ addMore) . (num %~ (+1)) $ model'

render :: Model -> Picture
render model = pictures . map (drawDot . (makeDot $ model^.time)) $ model^.angles

makeDot :: Float -> Float -> Dot
makeDot time angle = Dot (x, y) black
        where
            mult = 100 * sin (time + angle)
            x = mult * sin angle
            y = mult * cos angle

drawDot :: Dot -> Picture
drawDot dot = uncurry translate (dot^.pos) $ color (dot^.col) $ circleSolid 10