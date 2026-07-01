module PixiEvents(main) where

-- Demonstrates JS EVENTS driving Haskell: a canvas click calls a Haskell
-- callback (registered as a StablePtr and invoked via globalThis.__mhs.invoke,
-- which re-enters the evaluator as a fresh thread). Object handles live in the
-- page's userland registry, globalThis.pixiObjs.

import Foreign.StablePtr
import Foreign.Ptr

foreign import javascript
  "return pixiObjs.push(globalThis.__pixiApp) - 1"
  getApp :: IO Int

foreign import javascript
  "var app=pixiObjs[$0]; var g=new PIXI.Graphics(); g.beginFill($1); g.drawRect(-60,-60,120,120); g.endFill(); g.x=app.renderer.width/2; g.y=app.renderer.height/2; app.stage.addChild(g); return pixiObjs.push(g)-1"
  addBox :: Int -> Int -> IO Int

-- Install a canvas click listener that invokes the Haskell callback $1 with the
-- click x-coordinate. This runs long after `main` has returned.
foreign import javascript
  "var app=pixiObjs[$0]; app.view.addEventListener('click', function(e){ var r=app.view.getBoundingClientRect(); __mhs.invoke($1, (e.clientX-r.left)|0); })"
  onClick :: Int -> Ptr () -> IO ()

-- Haskell-driven scene mutation: rotate box $0 and log from Haskell.
foreign import javascript
  "var o=pixiObjs[$0]; o.rotation += 0.15; console.log('[haskell] click x=' + $1 + ' -> rotation ' + o.rotation.toFixed(2))"
  bump :: Int -> Int -> IO ()

onClickH :: Int -> Int -> IO ()
onClickH box x = bump box x

main :: IO ()
main = do
  app <- getApp
  box <- addBox app 0x66ccff
  sp  <- newStablePtr (onClickH box)
  onClick app (castStablePtrToPtr sp)
  putStrLn "PixiEvents: click the canvas — each click rotates the box FROM HASKELL"
