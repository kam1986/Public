module Screen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


type Screen () as screen =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(screen)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let radius = 20
    let colorData = [| for _ in 1 .. 160 * 144 -> Color() |]
    let mutable Screen = Unchecked.defaultof<Texture2D>

    do 
        screen.Content.RootDirectory <- "hello"
            

       
                
    
    override screen.Initialize() =
    
        spriteBatch <- new SpriteBatch(screen.GraphicsDevice, 50000)
        Screen <- new Texture2D(screen.GraphicsDevice, 160, 144)
        base.Initialize()
         
         // TODO: Add your initialization logic here
         
    override this.LoadContent() =
        
         // TODO: use screen.Content to load your game content here   
         // On Windows you can load any PNG file directly as Texture2D

         // Read more about MonoGame's Content Pipeline: https://docs.monogame.net/articles/tools/mgcb_editor.html
         // or install it with package manager console: [dotnet tool install -g dotnet-mgcb-editor]
        
        ()
 
    override this.Update gameTime =
        let diam = float32 radius / 2f
        let diam = diam * diam

        for x in 0 .. 160 do
            for y in 0 .. 144 do
                let index = x * radius + y
                let pos = new Vector2(float32 x - diam, float32 y - diam)
                if pos.LengthSquared() <= float32 radius then
                    colorData.[index] <- Color.Black
                else
                    colorData.[index] <- Color.Black

        Screen.SetData(colorData)
         // TODO: Add your update logic here
        base.Update gameTime
        ()
 
    override this.Draw gameTime =

        screen.GraphicsDevice.Clear Color.CornflowerBlue


        spriteBatch.Begin()
        spriteBatch.Draw(Screen, Vector2.Zero, Color.AliceBlue)
        spriteBatch.End()
