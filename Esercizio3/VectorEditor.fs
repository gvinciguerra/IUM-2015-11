module VectorEditorApp.Main

open System
open System.Drawing
open System.Diagnostics
open System.Windows.Forms
open System.Drawing.Drawing2D
open VectorEditorApp.LWCs
open VectorEditorApp.Shapes
open VectorEditorApp.Widgets

type NavBut = Up = 0 | Right = 1 | Left = 2 | Down = 3
type VectorEditorStates =
    /// Non ci sono azioni di disegno in corso, ma una forma potrebbe essere stata selezionata per la manipolazione.
    | Viewing
    /// È stata appena disegnata una curva ed è necessario impostarne la curvatura nel prossimo mouseDown.
    | DrawingCurvature 
    /// Una forma sta per essere disegnata o è già sullo schermo in attesa che venga confermata con un mouseUp.
    | DrawingShape     
    /// Deve essere creata e impostata la posizione iniziale della pallina con un mouseDown.
    | AddingBall       
    /// Deve essere confermata la velocità della pallina in posizione con un mouseUp.
    | ThrowingBall     

type VectorEditor(ToDeviceX, ToDeviceY) as this =
    inherit LWContainer()

    let mutable w2v = new Drawing2D.Matrix()
    let mutable v2w = new Drawing2D.Matrix()
    let mutable scrollDir = NavBut.Up

    let mutable selectedShape = None
    let mutable startingPoint = (new PointF(), new PointF()) // Il primo punto è in coordinate vista
    let mutable lastMouseWorld = new PointF() // L'ultima posizione del mouse registrata in un evento di trascinamento forma
    let mutable shapes = new ResizeArray<Shape>()
    let mutable shapeToDraw = None 
    let mutable currentState = Viewing

    // ASPETTO DELL'INTERFACCIA
    let barPadding = ToDeviceY 5.f
    let barButtonsSpacing = ToDeviceX 10.f
    let barButtonsSize = new SizeF(ToDeviceX 25.f, ToDeviceY 25.f)
    let barBrush = new SolidBrush(Color.FromArgb(83, 83, 83))
    let barLine  = new Pen(Color.FromArgb(110, 110, 110), ToDeviceY 1.f)
    let barLine2 = new Pen(Color.FromArgb(40, 40, 40), ToDeviceY 1.f)

    // ####################### MODIFICHE ALLA VISTA #######################
    let translateW (tx, ty) = w2v.Translate(tx, ty) ; v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)
    let rotateW a = w2v.Rotate a ; v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)
    let rotateAtW (p, a) =  w2v.RotateAt(a, p) ; v2w.RotateAt(-a, p, Drawing2D.MatrixOrder.Append)
    let scaleW (sx, sy) = w2v.Scale(sx, sy) ; v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)

    let transformP (m:Drawing2D.Matrix) (p:Point) =
        let a = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(a)
        a.[0]

    let scrollBy dir =
        match dir with
        | NavBut.Up -> (0.f, -10.f)
        | NavBut.Down -> (0.f, 10.f)
        | NavBut.Left -> (-10.f, 0.f)
        | _ -> (10.f, 0.f)

    let translate (x, y) =
        let t = [| PointF(0.f, 0.f); PointF(x, y) |]
        v2w.TransformPoints(t)
        translateW(t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)

    let scrollTimer = new Timer(Interval=100)
    do scrollTimer.Tick.Add(fun _ ->
        scrollBy scrollDir |> translate
        this.Invalidate()
    )

    let handleCommand (k:Keys) =
        match k with
        | Keys.W -> scrollBy NavBut.Up |> translate
        | Keys.A -> scrollBy NavBut.Left |> translate
        | Keys.S -> scrollBy NavBut.Down |> translate
        | Keys.D -> scrollBy NavBut.Right |> translate
        | Keys.Q -> let p = transformP v2w (Point(this.Width / 2, this.Height / 2)) in rotateAtW(p, 10.f)
        | Keys.E -> let p = transformP v2w (Point(this.Width / 2, this.Height / 2)) in rotateAtW(p, -10.f)
        | Keys.Z ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            scaleW(1.1f, 1.1f)
            let p1 = transformP v2w (Point(this.Width / 2, this.Height / 2))
            translateW(p1.X - p.X, p1.Y - p.Y)
        | Keys.X ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            scaleW(1.f/1.1f, 1.f / 1.1f)
            let p1 = transformP v2w (Point(this.Width / 2, this.Height / 2))
            translateW(p1.X - p.X, p1.Y - p.Y)
        | Keys.Delete | Keys.Back -> match selectedShape with Some s -> shapes.Remove(s) |> ignore | None -> ()
        | _ -> ()
        this.Invalidate()

    // ####################### MODIFICA SPESSORE LINEE #######################
    let textBox = new TextBox(Visible=false)
    do
        this.Controls.Add(textBox)
        textBox.KeyUp.Add(fun _ ->
            match selectedShape with
            | Some s ->
                try
                    let newWidth = System.Int32.Parse(textBox.Text)
                    if newWidth > 0 then s.PenWidth <- single newWidth
                    this.Invalidate()
                with _ -> ()

            | None -> textBox.Visible <- false
        )


    // ####################### GESTIONE DEI BOTTONI #######################
    let modifyViewButtons = [|
        new CuteButton(Text="⊕", Tag=0, ButtonType=Push, Parent=this);
        new CuteButton(Text="⊖", Tag=1, ButtonType=Push, Parent=this);
    |]
    do
        modifyViewButtons |> Seq.iter (fun b ->
            currentState <- Viewing
            b.Size <- barButtonsSize
            b.Click.Add(fun e -> if b.Tag = 0 then handleCommand Keys.Z else handleCommand Keys.X )
            this.LWControls.Add(b)
        )

    let ballButton = new CuteButton(Text=((char)0x26BD).ToString(), Tag=42, ButtonType=PushOnPushOff, Parent=this, Size=barButtonsSize);

    let shapeButtons = [| 
        new CuteButton(Text="/", Tag=Line, ButtonType=PushOnPushOff, Parent=this);
        new CuteButton(Text="∿", Tag=Curve, ButtonType=PushOnPushOff, Parent=this);
        new CuteButton(Text="⬭", Tag=Ellipse, ButtonType=PushOnPushOff, Parent=this);
        new CuteButton(Text="▭", Tag=Rectangle, ButtonType=PushOnPushOff, Parent=this);
    |]
    do
        shapeButtons |> Seq.iter (fun b ->
            b.Size <- barButtonsSize
            b.Click.Add(fun e ->
                shapeToDraw <- Some b.Tag
                currentState <- DrawingShape
                ballButton.Pressed <- false
                shapeButtons |> Seq.iter(fun s -> if s.Tag <> b.Tag then s.Pressed <- false)
            )
            this.LWControls.Add(b)
        )
        this.LWControls.Add(ballButton)
        ballButton.Click.Add(fun e ->
            shapeToDraw <- None
            currentState <- AddingBall
            shapeButtons |> Seq.iter(fun s -> s.Pressed <- false)
        )
    let barHeight = shapeButtons.[0].Size.Height + barPadding * 2.f

    
    // ####################### GESTIONE DELLE PALLINE #######################
    let mutable ballInPosition = None
    let balls = new ResizeArray<BouncingBall>()

    let updateBalls() =
        balls |> Seq.iter (fun b ->
            b.UpdatePosition()
            b.UpdateSpeed(balls)

            // Collisioni con pareti
            let edges1, edges2 = transformP v2w (Point(0, 0)), transformP v2w (Point(this.Width, this.Height - int barHeight))
            let leftEdge, topEdge = edges1.X, edges1.Y
            let rightEdge, bottomEdge = edges2.X, edges2.Y
            if b.Location.X <= leftEdge then
                b.Location <- new PointF(leftEdge, b.Location.Y)
                b.Speed <- SizeF(-b.Speed.Width, b.Speed.Height)
            else if b.Location.X + b.Size.Width >= rightEdge then
                b.Location <- new PointF(rightEdge - b.Size.Width, b.Location.Y)
                b.Speed <- SizeF(-b.Speed.Width, b.Speed.Height)
            if b.Location.Y <= topEdge then
                b.Location <- new PointF(b.Location.X, topEdge)
                b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)
            else if b.Location.Y + b.Size.Height >= bottomEdge then
                b.Location <- new PointF(b.Location.X, bottomEdge - b.Size.Height)
                b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)

            // Collisioni con forme
            shapes |> Seq.iter (fun s ->
                use pen = new Pen(Color.Black, b.Size.Width + s.PenWidth) 
                if s.Path.IsOutlineVisible(b.Center, pen) then
                    b.Speed <- SizeF(-b.Speed.Width, -b.Speed.Height)
                    b.ResetPreviousLocation()
            )
        )

    let timer = new Timer(Interval=25)
    do timer.Tick.Add(fun _  -> updateBalls() ; this.Invalidate()) ; timer.Start()

    // ####################### EVENTI DEL MOUSE #######################
    override this.OnMouseDown e =
        let mouseW = transformP v2w e.Location
        startingPoint <- (Point.op_Implicit e.Location, mouseW)

        match currentState with
        | DrawingCurvature ->  // Termina il disegno della curva
            (shapes.[0] :?> CurveShape).MidPoint <- Some mouseW
            shapeToDraw <- None
            shapeButtons.[Curve].Pressed <- false
            currentState <- Viewing
        | DrawingShape -> // Comincia il disegno della forma
            match shapeToDraw with
            | Some s -> 
                let newShape = ShapeFactory s
                newShape.HandleLineWidth <- ToDeviceX 1.f
                newShape.Location <- mouseW
                shapes.Insert(0, newShape)
            | _ -> ()
        | Viewing -> // Seleziona una forma
            for s in shapes do s.InEditingMode <- false
            let predicate (s : Shape) =  s.HitTest mouseW || s.HitTestRotate mouseW || s.IndexOfHandleAt(mouseW).IsSome
            let index = shapes |> Seq.tryFindIndex predicate
            match index with
                | Some i ->
                    selectedShape <- Some shapes.[i]
                    shapes.[i].InEditingMode <- true
                    textBox.Visible <- true
                    textBox.Text <- string(int(shapes.[i].PenWidth))
                | _ -> selectedShape <- None ; textBox.Visible <- false ; this.Focus() |> ignore
        | AddingBall ->
            let ball = new BouncingBall()
            ball.Location <- PointF(single mouseW.X - ball.Size.Width / 2.f, single mouseW.Y - ball.Size.Height / 2.f)
            ballInPosition <- Some ball
            lastMouseWorld <- mouseW
            currentState <- ThrowingBall
        | _ -> ()
            
        this.Invalidate()
        base.OnMouseDown e

    override this.OnMouseMove e =
        let worldStartingPoint, mouseW = snd startingPoint, transformP v2w e.Location

        match e.Button = MouseButtons.Left, currentState with
        | true, DrawingShape ->
            match shapeToDraw with
            | Some Line | Some Curve -> // Disegna Linea/Curva
                shapes.[0].Size <- SizeF(mouseW.X - worldStartingPoint.X, mouseW.Y - worldStartingPoint.Y)
                this.Invalidate()
            | Some s -> // Disegna Forma
                shapes.[0].Location <- PointF(Math.Min(mouseW.X, worldStartingPoint.X), Math.Min(mouseW.Y, worldStartingPoint.Y))
                shapes.[0].Size <- SizeF(Math.Abs(mouseW.X - worldStartingPoint.X), Math.Abs(mouseW.Y - worldStartingPoint.Y))
                this.Invalidate()
            | None -> currentState <- Viewing
        | true, Viewing ->
            match selectedShape with
            | Some s -> // Azioni su forma selezionata
                this.Cursor <- s.DragWith(worldStartingPoint, mouseW, lastMouseWorld)
                lastMouseWorld <- mouseW
                this.Invalidate()
            | None -> ()
        | false, DrawingCurvature -> // Mostra in anteprima la curvatura
            (shapes.[0] :?> CurveShape).MidPoint <- Some mouseW
            this.Invalidate()
        | true, ThrowingBall -> lastMouseWorld <- mouseW ; this.Invalidate()
        | _, _ -> ()
        base.OnMouseMove e

    override this.OnMouseUp e =
        match currentState, shapeToDraw with
        | DrawingShape, Some Curve -> currentState <- DrawingCurvature
        | DrawingShape, Some s -> currentState <- Viewing ; shapeToDraw <- None ; shapeButtons.[s].Pressed <- false
        | ThrowingBall, _ ->
            match ballInPosition with
            | Some b ->
                let vector = Point.Round(transformP v2w e.Location)
                b.ThrowBall(vector, 0.5f)
                balls.Add(b)
                ballButton.Pressed <- false 
                ballInPosition <- None
                currentState <- Viewing
                this.Invalidate()
            | None -> currentState <- Viewing ; Debug.Assert(false)
        | _, _ -> ()
        lastMouseWorld <- new PointF()
        this.Cursor <- Cursors.Default
        base.OnMouseUp e

    override this.OnKeyDown e =
        handleCommand e.KeyCode

    override this.OnResize e =
        base.OnResize(e)

        // Posiziona i pulsanti di modifica vista a sinistra
        let startX = barButtonsSpacing
        let y = single this.Height - modifyViewButtons.[0].Size.Height - barPadding
        let mutable x = startX
        for b in modifyViewButtons do
            b.Location <- new PointF(x, y)
            x <- x + b.Size.Width + barButtonsSpacing

        // Posiziona i pulsanti di disegno al centro
        let startX = 0.5f * (single this.Width - single shapeButtons.Length * (shapeButtons.[0].Size.Width + barButtonsSpacing))
        let mutable x = startX
        for b in shapeButtons do
            b.Location <- new PointF(x, y)
            x <- x + b.Size.Width + barButtonsSpacing

        // Posiziona la textBox e i pulsanti di gioco a destra
        let x = single this.Width - barButtonsSpacing - ballButton.Size.Width
        ballButton.Location <- new PointF(x, y)
        let x = int x - textBox.Width - int barButtonsSpacing
        let y = this.Height - (int barHeight + textBox.Height) / 2
        textBox.Location <- new Point(x, y)

    override this.OnPaint e =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        let mainCtx = g.Save()
        g.Transform <- w2v

        // Disegna tutte le forme
        shapes |> Seq.iter (fun s -> s.Paint g)

        // Disegna il vettore della pallina e palline
        match ballInPosition with
        | Some b ->
            use p = new Pen(Brushes.Black)
            p.EndCap <- Drawing2D.LineCap.ArrowAnchor
            g.DrawLine(p, b.Center, lastMouseWorld)
            b.Paint g
        | None -> ()            
        balls |> Seq.iter (fun b -> b.Paint g)

        // Disegna la barra
        let line2Y = single this.Height - barHeight
        let line1Y = line2Y + ToDeviceY 1.f
        g.Restore(mainCtx)
        g.FillRectangle(barBrush, 0.f, single this.Height - barHeight, single this.Width, barHeight)
        g.DrawLine(barLine, 0.f, line1Y, single this.Width, line1Y)
        g.DrawLine(barLine2, 0.f, line2Y, single this.Width, line2Y)
        base.OnPaint(e) 

[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern bool SetProcessDPIAware()

[<EntryPoint>]
let main args =    
    SetProcessDPIAware() |> ignore
 
    let f = new Form(Text="Vector Editor ~ Vinciguerra", TopMost=true, ShowIcon=false)

    // Crea le funzioni che convertono un punto (misura relativa) a un pixel (misura del dispositivo)
    let tmpG = f.CreateGraphics()
    let ratioX, ratioY = tmpG.DpiX / 96.f, tmpG.DpiY / 96.f
    tmpG.Dispose()
    let funX x = x * ratioX
    let funY y = y * ratioY

    let e = new VectorEditor(Dock=DockStyle.Fill, ToDeviceX=funX, ToDeviceY=funY)
    f.Controls.Add(e)
    f.Size <- new Size(int(funX 809.f), int(funY 500.f))
    f.MinimumSize <- new Size(int(funX 350.f), int(funY 299.f))
    f.StartPosition <- FormStartPosition.CenterScreen
    
    e.Focus() |> ignore
    Application.Run(f)
    0

