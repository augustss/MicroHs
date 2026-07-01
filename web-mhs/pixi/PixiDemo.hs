module PixiDemo(main) where

-- Object handles are kept in a small userland registry the page provides,
-- globalThis.pixiObjs. The page also puts the PIXI app on globalThis.__pixiApp.

-- Register the pre-created PIXI app and return its handle.
foreign import javascript
  "return pixiObjs.push(globalThis.__pixiApp) - 1"
  getApp :: IO Int

-- Draw a filled box of colour $1, centre it, add to app $0's stage; return its handle.
foreign import javascript
  "var app=pixiObjs[$0]; var g=new PIXI.Graphics(); g.beginFill($1); g.drawRect(-60,-60,120,120); g.endFill(); g.x=app.renderer.width/2; g.y=app.renderer.height/2; app.stage.addChild(g); return pixiObjs.push(g)-1"
  addBox :: Int -> Int -> IO Int

-- Install a per-frame ticker on app $0 that rotates object $1.
foreign import javascript
  "var app=pixiObjs[$0]; var o=pixiObjs[$1]; app.ticker.add(function(d){ o.rotation += 0.03*d; })"
  spin :: Int -> Int -> IO ()

main :: IO ()
main = do
  putStrLn "PixiDemo: Haskell driving pixi.js"
  app <- getApp
  box <- addBox app 0xff3366
  spin app box
  putStrLn "PixiDemo: scene set up, spinning."
