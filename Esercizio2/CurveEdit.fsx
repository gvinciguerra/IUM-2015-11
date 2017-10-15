#load "LWCs.fsx"

open System.Windows.Forms
open System.Drawing
open LWCs

type IumButton() as this =
    inherit LWC()

    let clickevt = new Event<System.EventArgs>()
    let downevt = new Event<MouseEventArgs>()
    let upevt = new Event<MouseEventArgs>()
    let moveevt = new Event<MouseEventArgs>()
    let mutable text = ""
    
    do this.Size <- SizeF(32.f, 32.f)

    member this.Click = clickevt.Publish
    member this.MouseDown = downevt.Publish
    member this.MouseUp = upevt.Publish
    member this.MouseMove = moveevt.Publish
    override this.DrawsInWorldCoordinates = true

    member this.Text
        with get() = text
        and set(v) = text <- v; this.Invalidate()

    override this.OnMouseUp e = upevt.Trigger(e); clickevt.Trigger(new System.EventArgs())

    override this.OnMouseMove e = moveevt.Trigger(e)

    override this.OnMouseDown e = downevt.Trigger(e)

    override this.OnPaint e =
        let g = e.Graphics
        let ellipseRect = new RectangleF(this.Location, this.Size)
        g.FillEllipse(Brushes.Red, ellipseRect)
        let strSize = g.MeasureString(text, this.Parent.Font)
        let strPos  = PointF(ellipseRect.Left + (ellipseRect.Width - strSize.Width) / 2.f, ellipseRect.Top + (ellipseRect.Height - strSize.Height) / 2.f)
        g.DrawString(text, this.Parent.Font, Brushes.White, strPos)

type NavBut =
| Up = 0
| Right = 1
| Left = 2
| Down = 3

type Editor() as this =
    inherit LWContainer()

    let pts = [| PointF(); PointF(20.f, 20.f); PointF(50.f, 50.f); PointF(50.f, 100.f) |]

    let buttons = [| 
        new IumButton(Text="U",Location=PointF(32.f, 0.f));
        new IumButton(Text="R",Location=PointF(64.f, 32.f));
        new IumButton(Text="L",Location=PointF(0.f, 32.f));
        new IumButton(Text="D",Location=PointF(32.f, 64.f));
    |]

    let button (k:NavBut) =
        buttons.[int(k)]

    let handleSize = 5.f

    let mutable selected = None
    let mutable offsetDrag = PointF()

    let mutable tension = 1.f

    let mutable w2v = new Drawing2D.Matrix()
    let mutable v2w = new Drawing2D.Matrix()

    let translateW (tx, ty) =
        w2v.Translate(tx, ty)
        v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

    let rotateW a =
        w2v.Rotate a
        v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)

    let rotateAtW p a =
        w2v.RotateAt(a, p)
        v2w.RotateAt(-a, p, Drawing2D.MatrixOrder.Append)

    let scaleW (sx, sy) =
        w2v.Scale(sx, sy)
        v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)

    let transformP (m:Drawing2D.Matrix) (p:Point) =
        let a = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(a)
        a.[0]

    let handleHitTest (p:PointF) (h:PointF) =
        let x = p.X - h.X
        let y = p.Y - h.Y
        x * x + y * y < handleSize * handleSize

    do buttons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))

    let scrollBy dir =
        match dir with
        | NavBut.Up -> (0.f, -10.f)
        | NavBut.Down -> (0.f, 10.f)
        | NavBut.Left -> (-10.f, 0.f)
        | NavBut.Right -> (10.f, 0.f)

    let translate (x, y) =
        let t = [| PointF(0.f, 0.f); PointF(x, y) |]
        v2w.TransformPoints(t)
        translateW(t.[1].X - t.[0].X, t.[1].Y - t.[0].Y)
    
    let handleCommand (k:Keys) =
        match k with
        | Keys.W ->
            scrollBy NavBut.Up |> translate
            this.Invalidate()
        | Keys.A ->
            scrollBy NavBut.Left |> translate
            this.Invalidate()
        | Keys.S ->
            scrollBy NavBut.Down |> translate
            this.Invalidate()
        | Keys.D ->
            scrollBy NavBut.Right |> translate
            this.Invalidate()
        | Keys.Q ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            rotateAtW p 10.f
            this.Invalidate()
        | Keys.E ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            rotateAtW p -10.f
            this.Invalidate()
        | Keys.Z ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            scaleW(1.1f, 1.1f)
            let p1 = transformP v2w (Point(this.Width / 2, this.Height / 2))
            translateW(p1.X - p.X, p1.Y - p.Y)
            this.Invalidate()
        | Keys.X ->
            let p = transformP v2w (Point(this.Width / 2, this.Height / 2))
            scaleW(1.f/1.1f, 1.f / 1.1f)
            let p1 = transformP v2w (Point(this.Width / 2, this.Height / 2))
            translateW(p1.X - p.X, p1.Y - p.Y)
            this.Invalidate()
        | _ -> ()

    let scrollTimer = new Timer(Interval=100)
    let mutable scrollDir = NavBut.Up

    do scrollTimer.Tick.Add(fun _ ->
        scrollBy scrollDir |> translate
        this.Invalidate()
    )

    do 
        buttons.[int(NavBut.Up)].MouseDown.Add(fun _ -> scrollDir <- NavBut.Up)
        buttons.[int(NavBut.Down)].MouseDown.Add(fun _ -> scrollDir <- NavBut.Down)
        buttons.[int(NavBut.Left)].MouseDown.Add(fun _ -> scrollDir <- NavBut.Left)
        buttons.[int(NavBut.Right)].MouseDown.Add(fun _ -> scrollDir <- NavBut.Right)
        for v in [ NavBut.Up; NavBut.Left; NavBut.Right; NavBut.Down] do
            let idx = int(v)
            buttons.[idx].MouseDown.Add(fun _ -> scrollTimer.Start()) //sono due call per MouseDown!
            buttons.[idx].MouseUp.Add(fun _ -> scrollTimer.Stop())

    member this.V2W = v2w

    member this.Tension
        with get () = tension
        and set (v) = tension <- v; this.Invalidate()

    override this.OnMouseDown e =
        base.OnMouseDown(e)
        let l = transformP v2w e.Location
        let ht = handleHitTest l
        selected <- pts |> Array.tryFindIndex ht
        match selected with
        | Some(idx) ->
            let p = pts.[idx]
            offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
        | None -> ()

    override this.OnMouseUp e =
        base.OnMouseUp e
        selected <- None

    override this.OnMouseMove e =
        let l = transformP v2w e.Location
        match selected with
        | Some idx -> 
            pts.[idx] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
            this.Invalidate()
        | None -> ()

    override this.OnPaint e =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        let drawHandle (p:PointF) =
            let w = 5.f
            g.DrawEllipse(Pens.Black, p.X - w, p.Y - w, 2.f * w, 2.f * w)
        let ctx = g.Save()
        g.Transform <- w2v
        g.DrawBezier(Pens.Black, pts.[0], pts.[1], pts.[2], pts.[3])
        g.DrawLine(Pens.Red, pts.[0], pts.[1])
        g.DrawLine(Pens.Red, pts.[2], pts.[3])
        g.DrawCurve(Pens.Blue, pts, tension)
        // let (|>) x f = f x
        pts |> Array.iter drawHandle
        g.Restore(ctx)
        base.OnPaint(e)
 
    override this.OnKeyDown e =
        handleCommand e.KeyCode

let f = new Form(Text="Curve editor", TopMost=true)
let e = new Editor(Dock=DockStyle.Fill)
f.Controls.Add(e)
f.Show()
e.Focus()
Application.Run(f)