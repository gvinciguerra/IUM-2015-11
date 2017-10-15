module VectorEditorApp.Shapes
open VectorEditorApp.LWCs
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System

[<AbstractClass>]
type Shape() =
    let mutable inEditingMode = false
    let mutable color = Color.Black
    let mutable editingColor = Color.DarkRed
    let mutable size = new SizeF()
    let mutable location = new PointF()
    let mutable lastStartingPoint = PointF()
    let handleRadius = 15.f
    let rotateHandleColor = Brushes.DarkBlue
    let mutable rotating = false
    let mutable rotateHandleRect = RectangleF()
    let mutable angle = 0.f
    let mutable handleLineWidth = 2.f
    let rotationMatrix = new Matrix()
    let reverseRotationMatrix = new Matrix()
    let mutable penWidth = 2.f
    let defaultPen = new Pen(color, penWidth)
    let defaultEditingPen = new Pen(editingColor, penWidth, DashPattern=[| 2.f ; 3.f |])

    /// Un array di punti che indica la posizine delle maniglie di manipolazione delle dimensioni della forma. Non è affetto da eventuali rotazioni della forma.
    abstract HandlesArray : PointF[]

    /// Indica se la forma è in modalità manipolazione.
    member this.InEditingMode with get() = inEditingMode and set(v) = inEditingMode <- v

    /// Restituisce l'oggetto Pen più appropriato per disegnare la forma in base al suo stato.
    abstract member CurrentPen : Pen with get
    default this.CurrentPen with get() = if this.InEditingMode then defaultEditingPen else defaultPen

    abstract member PenWidth : single with get, set
    default this.PenWidth
        with get() = penWidth
        and set(v) = penWidth <- v ; defaultPen.Width <- v ; defaultEditingPen.Width <- v

    abstract member UpdatePath : unit -> unit

    abstract member Location : PointF  with get, set
    default this.Location with get() = location and set(v) = location <- v ; this.UpdatePath()

    abstract member Size : SizeF  with get, set
    default this.Size with get() = size and set(v) = size <- v ; this.UpdatePath()

    abstract member HandleLineWidth : single with get, set
    default this.HandleLineWidth with get() = handleLineWidth and set(v) = handleLineWidth <- v

    abstract member Rectangle : Rectangle with get
    default this.Rectangle = new Rectangle(int this.X, int this.Y, int this.Width, int this.Height)

    abstract member RectangleF : RectangleF with get, set
    default this.RectangleF with get() = new RectangleF(this.Location, this.Size) and set(r) = this.Location <- r.Location ; this.Size <- r.Size ; this.UpdatePath()

    abstract member Path : GraphicsPath  with get
 
    abstract member Region : Region
    default this.Region with get() = new Region(this.Path)

    member this.Angle
        with get() = angle
        and set(v) =
            angle <- v % 360.f
            rotationMatrix.Reset()
            rotationMatrix.RotateAt(angle, this.Center)
            reverseRotationMatrix.Reset()
            reverseRotationMatrix.RotateAt(-angle, this.Center, Drawing2D.MatrixOrder.Append)

    member this.ApplyRotation p : PointF = 
        let a = [| p |]
        reverseRotationMatrix.TransformPoints(a : PointF[])
        a.[0]

    member this.X = this.Location.X
    member this.Y = this.Location.Y
    member this.Width = this.Size.Width
    member this.Height = this.Size.Height
    member this.Right = this.Location.X + this.Size.Width
    member this.Bottom = this.Location.Y + this.Size.Height
    member this.Center = new PointF(this.X + this.Width / 2.f, this.Y + this.Height / 2.f)
    member this.RotationMatrix = rotationMatrix

    abstract member Color : Color  with get, set
    default this.Color with get() = color and set(v) = color <- v

    /// Indica se il punto specificato ricade o meno nella forma.
    abstract member HitTest : PointF -> bool
    default this.HitTest pt =
        let p = this.ApplyRotation pt
        use path = new GraphicsPath()
        path.AddRectangle(this.RectangleF)
        path.IsVisible(p)
    
    /// Restituisce l'indice della maniglia in HandlesArray a cui il punto appartiene.
    member this.IndexOfHandleAt(pt : PointF) : option<int> =
        let p = this.ApplyRotation pt
        let predicate (h : PointF) = (h.X - p.X) ** 2.f + (h.Y - p.Y) ** 2.f <= handleRadius ** 2.f
        this.HandlesArray |> Array.tryFindIndex predicate

    /// Disegna la forma usando la superficie specificata.
    abstract member Paint : Graphics -> unit
        
    /// Disegna tutte le maniglie ai punti di HandlesArray, più la maniglia di rotazione.
    member this.PaintAllHandles (g : Graphics) =
        if this.InEditingMode then
            let ctx = g.Save()
            g.MultiplyTransform(rotationMatrix)
            this.HandlesArray |> Seq.iter (fun h -> this.PaintHandleAt(h, g))
            rotateHandleRect <- new RectangleF(this.Right + handleRadius, this.Center.Y - handleRadius / 2.f, handleRadius, handleRadius)
            use pen = new Pen(rotateHandleColor, handleLineWidth)
            pen.StartCap <- LineCap.ArrowAnchor
            pen.EndCap <- LineCap.ArrowAnchor
            g.DrawArc(pen, rotateHandleRect, -90.f, 180.f) 
            g.Restore(ctx)

    /// Indica se al punto specificato è presente la maniglia di rotazione della forma.
    member this.HitTestRotate (pt : PointF) =
        let p = this.ApplyRotation pt
        rotateHandleRect.Contains(p)
        
    /// Calcola la distanza tra due punti tenendo conto della rotazione dell'oggetto.
    member this.RotatedDistance(arg1, arg2) =
        let p1 = this.ApplyRotation arg1
        let p2 = this.ApplyRotation arg2
        new PointF(p2.X - p1.X, p2.Y - p1.Y) 

    /// Esegue un'azione di trascinamento/rotazione sulla forma e restituisce il cursore più appropriato.  I tre argomenti indicano  la posizione di inizio del trascinamento, la posizione corrente del mouse, la posizione nella precedente chiamata. L'implementazione di default ignora le maniglie di HandlesArray, ma gestisce lo spostamento e la rotazione della forma.
    abstract member DragWith : PointF * PointF * PointF -> Cursor
    default this.DragWith(startingPoint, mouseWorld, lastMouseWorld) =
        match rotating, startingPoint = lastStartingPoint with
        | _, false ->
            rotating <- this.HitTestRotate startingPoint
            lastStartingPoint <- startingPoint
            Cursors.Default
        | true, true -> // Rotazione
            let shapeCenter = this.Center
            let x, y = float(mouseWorld.X - shapeCenter.X), float(mouseWorld.Y - shapeCenter.Y)
            let thetaRad = single(Math.Atan2(y, x))
            let pi = single Math.PI
            this.Angle <- thetaRad * 180.f / pi
            this.UpdatePath()
            Cursors.SizeAll
        | _, _ -> // Trascinamento
            let o = this.RotatedDistance(lastMouseWorld, mouseWorld) 
            this.Location <- new PointF(this.X + o.X, this.Y + o.Y)
            Cursors.SizeAll

    /// Disegna una maniglia standard per manipolare le dimensioni della forma al punto specificato.
    member private this.PaintHandleAt ((p : PointF), (g : Graphics)) =
        use handlePen = new Pen(Color.Orange, handleLineWidth)
        g.DrawEllipse(handlePen, p.X - handleRadius / 2.f, p.Y - handleRadius / 2.f, handleRadius, handleRadius)

and CurveShape() =
    inherit Shape()

    let mutable lastStartingPoint = PointF()
    let mutable draggedHandle = None
    let mutable path = new GraphicsPath()
    let mutable midPoint = None

    override this.UpdatePath() =
        path.Reset()
        match this.MidPoint with
        | Some p -> path.AddBezier(this.Location, p, p, new PointF(this.Right, this.Bottom))
        | None -> path.AddLine(this.X, this.Y, this.Right, this.Bottom)
        path.Transform(this.RotationMatrix)

    override this.Path with get() = path

    /// Un valore diverso da None rappresenta la curvatura. Un valore uguale a None permette di disegnare una linea.
    member this.MidPoint with get() = midPoint and set(v) = midPoint <- v ; this.UpdatePath()

    override this.HandlesArray =
        match this.MidPoint with
        | Some p -> [| this.Location ; p ; PointF.Add(this.Location, this.Size) |]
        | None ->   [| this.Location ; PointF.Add(this.Location, this.Size) |]

    override this.Paint g =
        g.DrawPath(this.CurrentPen, path)
        base.PaintAllHandles g

    override this.HitTest pt =
        let p = this.ApplyRotation pt
        use biggerPen = new Pen(Color.Black)
        biggerPen.Width <- this.CurrentPen.Width * 2.f
        path.IsOutlineVisible(p, biggerPen)

    override this.DragWith(startingPoint, mouseWorld, lastMouseWorld) =
        let resizeWithHandle h =
            let o = this.RotatedDistance(lastMouseWorld, mouseWorld)
            match this.MidPoint, h with
            | _ , 0 -> this.RectangleF <- new RectangleF(this.X + o.X, this.Y + o.Y, this.Width - o.X, this.Height - o.Y)
            | Some p, 1 -> this.MidPoint <- Some (new PointF(p.X + + o.X, p.Y + o.Y))
            | _, _ -> this.RectangleF <- new RectangleF(this.X, this.Y, this.Width + o.X, this.Height + o.Y)
        match draggedHandle, startingPoint = lastStartingPoint with
        | Some h, true -> // Manipola tramite la maniglia h
            resizeWithHandle h
            Cursors.SizeAll
        | None, true -> // Azione di default, sposta l'oggetto
            base.DragWith(startingPoint, mouseWorld, lastMouseWorld) 
        | _, false -> // Nuovo startingPoint del trascinamento. Controlla se è stata colpita una maniglia
            draggedHandle <- this.IndexOfHandleAt(startingPoint)
            lastStartingPoint <- startingPoint ; Cursors.Default

and RectangleShape() =
    inherit Shape()

    let mutable lastStartingPoint = PointF()
    let mutable draggedHandle = None
    let mutable path = new GraphicsPath()

    override this.HandlesArray =  [| this.Location ;  new PointF(this.Right, this.Y) ;  new PointF(this.Right, this.Bottom) ; new PointF(this.X, this.Bottom) |] // Dall'angolo in alto a sinistra, in senso orario

    override this.UpdatePath() =
        path.Reset()
        let pts = this.HandlesArray
        path.AddLine(pts.[0], pts.[1])
        path.AddLine(pts.[1], pts.[2])
        path.AddLine(pts.[2], pts.[3])
        path.CloseFigure()
        path.Transform(this.RotationMatrix)

    override this.Path with get() = path

    override this.Paint g =
        g.DrawPath(this.CurrentPen, path)
        base.PaintAllHandles g

    override this.DragWith(startingPoint, mouseWorld, lastMouseWorld) =
        let cursorForHandle i =
            match i with
            | 0 | 2 -> Cursors.SizeNWSE
            | _ -> Cursors.SizeNESW
        let resizeWithHandle h =
            let o = this.RotatedDistance(lastMouseWorld, mouseWorld)
            match h with
            | 0 -> this.RectangleF <- new RectangleF(this.X + o.X, this.Y + o.Y, this.Width - o.X, this.Height - o.Y)
            | 1 -> this.RectangleF <- new RectangleF(this.X, this.Y + o.Y, this.Width + o.X, this.Height - o.Y)
            | 2 -> this.RectangleF <- new RectangleF(this.X, this.Y, this.Width + o.X, this.Height + o.Y)
            | _ -> this.RectangleF <- new RectangleF(this.X + o.X, this.Y, this.Width - o.X, this.Height + o.Y)
        match draggedHandle, startingPoint = lastStartingPoint with
        | Some h, true -> // Manipola tramite la maniglia h
            resizeWithHandle h
            cursorForHandle h
        | None, true -> // Azione di default, sposta l'oggetto
            base.DragWith(startingPoint, mouseWorld, lastMouseWorld) 
        | _, false -> // Nuovo startingPoint del trascinamento. Controlla se è stata colpita una maniglia
            draggedHandle <- this.IndexOfHandleAt(startingPoint)
            lastStartingPoint <- startingPoint ; Cursors.Default

and EllipseShape() =
    inherit Shape()

    let mutable lastStartingPoint = PointF()
    let mutable draggedHandle = None
    let mutable path = new GraphicsPath()

    override this.HandlesArray = [| new PointF(this.X + this.Width / 2.f, this.Y) ; new PointF(this.Right, this.Y + this.Height / 2.f) ; new PointF(this.X + this.Width / 2.f, this.Bottom) ; new PointF(this.X, this.Y + this.Height / 2.f) |]  // Dall'alto in senso orario

    override this.UpdatePath() =
        path.Reset()
        path.AddEllipse(this.RectangleF)
        path.Transform(this.RotationMatrix)

    override this.Path with get() = path

    override this.HitTest pt =
        let p = this.ApplyRotation pt        
        path.IsVisible(p)

    override this.DragWith(startingPoint, mouseWorld, lastMouseWorld) =
        let cursorForHandle i = match i with | 0 | 2 -> Cursors.SizeNS | _ -> Cursors.SizeWE
        let resizeWithHandle h =
            let o = this.RotatedDistance(lastMouseWorld, mouseWorld)
            match h with
            | 0 -> this.RectangleF <- new RectangleF(this.X, this.Y + o.Y, this.Width, this.Height - o.Y)
            | 1 -> this.RectangleF <- new RectangleF(this.X, this.Y, this.Width + o.X, this.Height)
            | 2 -> this.RectangleF <- new RectangleF(this.X, this.Y, this.Width, this.Height + o.Y)
            | _ -> this.RectangleF <- new RectangleF(this.X + o.X, this.Y, this.Width - o.X, this.Height)
        match draggedHandle, startingPoint = lastStartingPoint with
        | Some h, true -> // Manipola tramite la maniglia h
            resizeWithHandle h
            cursorForHandle h
        | None, true -> // Azione di default, sposta l'oggetto
            base.DragWith(startingPoint, mouseWorld, lastMouseWorld) 
        | _, false -> // Nuovo startingPoint del trascinamento. Controlla se è stata colpita una maniglia
            draggedHandle <- this.IndexOfHandleAt(startingPoint)
            lastStartingPoint <- startingPoint ; Cursors.Default

    override this.Paint g =
        g.DrawPath(this.CurrentPen, path)
        base.PaintAllHandles g

[<Literal>]
let Line = 0

[<Literal>]
let Curve = 1

[<Literal>]
let Ellipse = 2

[<Literal>]
let Rectangle = 3

let ShapeFactory t =
    match t with
    | Line -> new CurveShape() :> Shape
    | Curve -> new CurveShape() :> Shape
    | Ellipse -> new EllipseShape() :> Shape
    | _ -> new RectangleShape() :> Shape