namespace Game

/// Processing style vector type
/// See http://processing.org/reference/PVector.html
type PVector(x:float,y:float) =
    let mutable x = x
    let mutable y = y
    new(x:int,y:int) = PVector(float x,float y)
    member this.X 
        with get () = x
        and set x' = x <- x'
    member this.Y 
        with get () = y
        and set y' = y <- y'
    member vector.Mag() : float =
        sqrt(x * x + y * y)
    member vector.Normalize() : unit =
        let length = vector.Mag()
        vector.X <- vector.X / length
        vector.Y <- vector.Y / length
    member vector.Mult(n:float) : unit = 
        vector.X <- vector.X * n
        vector.Y <- vector.Y * n
    member this.Dot(that:PVector) : float =
        this.X * that.X + this.Y * that.Y
    static member Sub(lhs:PVector,rhs:PVector) : PVector =
        PVector(lhs.X - rhs.X, lhs.Y - rhs.Y)
    static member Mult(vector:PVector, n:float) : PVector =
        PVector(vector.X * n, vector.Y * n)

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PVector =
    let componentVector(vector:PVector, direction:PVector) =
        let v = PVector(direction.X, direction.Y)
        v.Normalize()
        v.Mult(vector.Dot(v))
        v
    let resolveFixedCollision(vector:PVector, direction:PVector) =
        let c = PVector.Mult(componentVector(vector, direction), 2.0)
        let v = PVector.Sub(vector, c)
        vector.X <- v.X
        vector.Y <- v.Y

type Ball (width:float, height:float) =

    let random =
        let generator = new System.Random()
        fun (from, until) -> generator.Next(until-from) + from

    let radius = 6.0
    let gravity = 0.1
    let position = PVector(width / 2.0, 20.0)
    let velocity = PVector(random(-2,2), random(1,2))

    (**
     * WALL COLLISION DECTECTION STUFF
     * this function takes 3 parameters, and uses the below other 2 functions
     * 1st position vector
     * 2nd vector starting point
     * 3rd vector slope
     *)
    let circleWallCollision(position:PVector, wallStartPoint:PVector, wall:PVector) =
        let mutable collided = false
        // variables to check if position vector is inside slope bounds
        let d1x = position.X - wallStartPoint.X
        let d1y = position.Y - wallStartPoint.Y
        let d2x = position.X - (wallStartPoint.X + wall.X)
        let d2y = position.Y - (wallStartPoint.Y + wall.Y)
        // check if the position of the ball is inside the boundaries of the wall (slope)
        // if so do the collision stuff
        if sqrt(d1x * d1x + d1y * d1y) < wall.Mag() &&
           sqrt(d2x * d2x + d2y * d2y) < wall.Mag()
        then
            let wallNormal = PVector(-wall.Y, wall.X)
            wallNormal.Normalize()
            // penetration factor
            let a = PVector.Sub(wallStartPoint, position).Dot(wallNormal)
            // check for collision
            if abs(a) < radius then
                resolveFixedCollision(velocity, wallNormal)
                collided <- true
        collided

    // STANDARD STAGE COLLISION DECTECTION
    let checkStageBounds () =
        let mutable collided = false
        if position.X + radius > float width then 
            position.X <- float width - radius
            velocity.X <- velocity.X * -1.0           
            collided <- true
        elif position.X - radius < 0.0 then
            position.X <- radius
            velocity.X <- velocity.X * -1.0
            collided <- true
        if position.Y + radius > float height then
            position.Y <- float height - radius
            velocity.Y <- velocity.Y * -1.0
            collided <- true
        elif position.Y - radius < 0.0 then
            position.Y <- radius
            velocity.Y <- velocity.Y * -1.0
            collided <- true
        collided

    member ball.Radius = radius

    member ball.Update (walls:(float * float * float * float)[]) =
        velocity.Y <- velocity.Y + gravity
        position.X <- position.X + velocity.X
        position.Y <- position.Y + velocity.Y

        let hitWall =
            walls |> Seq.exists (fun (x1,y1,x2,y2) ->
            let slope = PVector(x2 - x1, y2 - y1)
            circleWallCollision(position, PVector(x1,y1), slope);
        )
        let hitBorder = checkStageBounds ()
    
        hitBorder, hitWall, position.X, position.Y